-- |
-- Module      : Roon.Effect.Moo
-- Description : Requests and subscriptions multiplexed over one connection
-- License     : BSD-3-Clause
--
-- Many exchanges share one WebSocket. Each request carries an identifier
-- that its replies repeat; a request gets any number of @CONTINUE@
-- replies, then at most one @COMPLETE@. The server also sends requests of
-- its own, such as pings, which the extension must answer.
--
-- 'runMoo' reads the socket in a background thread and routes each reply
-- to the exchange that waits for it; another thread answers the server's
-- requests.
module Roon.Effect.Moo
  ( -- * Effect
    Moo
  , Exchange
  , Reply (..)
  , openExchange
  , nextReply
  , nextReplyWithin
  , closeExchange
  , newSubscriptionKey

    -- * Requests and subscriptions
  , request
  , requestWithin
  , defaultRequestTimeout
  , withSubscription

    -- * Interpreter
  , runMoo
  , RequestHandler
  , defaultRequestHandler
  , pingService
  ) where

import Control.Monad (forever, void, (>=>))
import Data.Bool (bool)
import Data.Aeson (object, (.=))
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Word (Word64)
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.Async (withAsync)
import Effectful.Concurrent.STM
  ( STM
  , TQueue
  , TVar
  , atomically
  , modifyTVar'
  , newTQueue
  , newTQueueIO
  , newTVar
  , newTVarIO
  , orElse
  , readTQueue
  , readTVar
  , registerDelay
  , retry
  , writeTQueue
  , writeTVar
  )
import Effectful.Dispatch.Dynamic (interpret_, send)
import Effectful.Error.Static (Error, throwError)
import Effectful.Exception (finally)
import Roon.Effect.WebSocket (WebSocket, frames, sendFrame)
import Roon.Error (RoonError (..))
import Roon.Moo.Message
  ( Body (..)
  , MooMessage (..)
  , RequestId
  , Verb (..)
  , encode
  , parse
  , requestIdFromWord
  , splitServiceMethod
  )
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream (Stream)
import Streamly.Data.Stream qualified as Stream

-- | A reply to a request.
data Reply = Reply
  { final :: Bool
  -- ^ 'True' for @COMPLETE@: no other reply will follow.
  , name :: Text
  -- ^ The status, such as @Success@.
  , body :: Body
  }
  deriving stock (Eq, Show)

-- | A request that has been sent, and the replies it has received so far.
data Exchange = Exchange
  { requestId :: RequestId
  , name :: Text
  -- ^ The request name, for errors.
  , replies :: TQueue Reply
  , finished :: TVar Bool
  }

-- | Exchange MOO messages with a Roon Server.
data Moo :: Effect where
  -- The failures are returned rather than thrown: an error thrown by the
  -- interpreter would reach the Error handler around 'runMoo', not the one
  -- of the caller.
  OpenExchange :: Text -> Body -> Moo m (Either RoonError Exchange)
  NextReply :: Maybe Int -> Exchange -> Moo m (Either RoonError (Maybe Reply))
  CloseExchange :: Exchange -> Moo m ()
  NewSubscriptionKey :: Moo m Word64

type instance DispatchOf Moo = Dynamic

-- | Send a request, named @service/method@, and return the handle to read
-- its replies. Throws if the connection is closed.
openExchange :: (Moo :> es, Error RoonError :> es) => Text -> Body -> Eff es Exchange
openExchange name body = send (OpenExchange name body) >>= either throwError pure

-- | Wait for the next reply. 'Nothing' means the exchange is over: its
-- final reply has been read, or it was closed. Throws if the connection
-- closes first.
nextReply :: (Moo :> es, Error RoonError :> es) => Exchange -> Eff es (Maybe Reply)
nextReply = nextReplyBefore Nothing

-- | 'nextReply', waiting at most a number of microseconds. Throws
-- 'TimedOut' when no reply comes in time; the exchange stays open.
nextReplyWithin :: (Moo :> es, Error RoonError :> es) => Int -> Exchange -> Eff es (Maybe Reply)
nextReplyWithin = nextReplyBefore . Just

nextReplyBefore :: (Moo :> es, Error RoonError :> es) => Maybe Int -> Exchange -> Eff es (Maybe Reply)
nextReplyBefore limit exchange = send (NextReply limit exchange) >>= either throwError pure

-- | Stop listening to an exchange. Replies that arrive later are dropped.
-- This does not tell the server anything.
closeExchange :: Moo :> es => Exchange -> Eff es ()
closeExchange = send . CloseExchange

-- | A number not yet used as a subscription key on this connection.
-- Services tie a subscription to the key given when it is opened, and
-- expect the same key to end it.
newSubscriptionKey :: Moo :> es => Eff es Word64
newSubscriptionKey = send NewSubscriptionKey

-- | Send a request and wait for its final reply, at most
-- 'defaultRequestTimeout'. Intermediate replies are discarded.
request :: (Moo :> es, Error RoonError :> es) => Text -> Body -> Eff es Reply
request = requestWithin (Just defaultRequestTimeout)

-- | Thirty seconds, in microseconds.
defaultRequestTimeout :: Int
defaultRequestTimeout = 30_000_000

-- | 'request' with another limit, in microseconds, for each reply;
-- 'Nothing' waits as long as needed. Throws 'TimedOut' when a reply does
-- not come in time.
requestWithin :: (Moo :> es, Error RoonError :> es) => Maybe Int -> Text -> Body -> Eff es Reply
requestWithin limit name body = do
  exchange <- openExchange name body
  await exchange `finally` closeExchange exchange
  where
    await exchange =
      nextReplyBefore limit exchange >>= \case
        Just reply
          | reply.final -> pure reply
          | otherwise -> await exchange
        Nothing -> throwError ConnectionLost

-- | Send a request and consume its replies as a stream, which ends after
-- the final reply. The exchange is closed when the consumer returns.
withSubscription
  :: (Moo :> es, Error RoonError :> es)
  => Text
  -> Body
  -> (Stream (Eff es) Reply -> Eff es a)
  -> Eff es a
withSubscription name body consume = do
  exchange <- openExchange name body
  consume (Stream.unfoldrM (const (fmap (,()) <$> nextReply exchange)) ())
    `finally` closeExchange exchange

-- | Answers a request made by the server. Receives the service, the
-- method and the body; returns the status and the body of the reply.
--
-- The handler runs in a thread of its own, one request after the other,
-- so that a slow answer does not hold up the replies to the extension.
-- Changes it makes to thread-local effects, such as a local @State@, are
-- not seen by the action given to 'runMoo': share data through a
-- shared-state effect or a 'TVar'.
type RequestHandler es = Text -> Text -> Body -> Eff es (Text, Body)

-- | The service every extension must provide: the server calls it to
-- check that the extension is alive.
pingService :: Text
pingService = "com.roonlabs.ping:1"

-- | Answers pings and rejects everything else.
defaultRequestHandler :: RequestHandler es
defaultRequestHandler service method _
  | service /= pingService = invalid ("unknown service: " <> service)
  | method /= "ping" = invalid ("unknown request name (" <> service <> ") : " <> method)
  | otherwise = pure ("Success", NoBody)
  where
    invalid reason = pure ("InvalidRequest", JsonBody (object ["error" .= (reason :: Text)]))

data MooState = MooState
  { nextId :: TVar Word64
  , pending :: TVar (Map RequestId Exchange)
  , closed :: TVar (Maybe RoonError)
  -- ^ Why no more reply will come, once the receive loop has stopped.
  , nextSubscriptionKey :: TVar Word64
  , serverRequests :: TQueue MooMessage
  -- ^ Requests of the server, waiting for the handler.
  }

-- | Run MOO exchanges over a WebSocket. The handler answers the requests
-- that the server makes.
runMoo
  :: (WebSocket :> es, Concurrent :> es)
  => RequestHandler es
  -> Eff (Moo : es) a
  -> Eff es a
runMoo handler action = do
  state <- MooState <$> newTVarIO 0 <*> newTVarIO Map.empty <*> newTVarIO Nothing <*> newTVarIO 0 <*> newTQueueIO
  withAsync (receiveLoop state) $ \_ ->
    withAsync (answerLoop state handler) $ \_ ->
      interpret_ (handleMoo state) action

handleMoo
  :: (WebSocket :> es, Concurrent :> es)
  => MooState
  -> Moo (Eff localEs) a
  -> Eff es a
handleMoo state = \case
  OpenExchange name body -> do
    opened <- atomically $ do
      readTVar state.closed >>= \case
        Just reason -> pure (Left reason)
        Nothing -> do
          requestId <- requestIdFromWord <$> readTVar state.nextId
          modifyTVar' state.nextId (+ 1)
          exchange <- Exchange requestId name <$> newTQueue <*> newTVar False
          modifyTVar' state.pending (Map.insert requestId exchange)
          pure (Right exchange)
    case opened of
      Left reason -> pure (Left reason)
      Right exchange -> do
        sent <- sendFrame (encode MooMessage {verb = Request, name, requestId = exchange.requestId, headers = Map.empty, body})
        if sent
          then pure (Right exchange)
          else Left ConnectionLost <$ atomically (forget state exchange)
  NextReply limit exchange -> do
    expired <- maybe (pure Nothing) (fmap Just . registerDelay) limit
    let timer = maybe retry (readTVar >=> bool retry (pure (Left (TimedOut exchange.name)))) expired
    atomically $
        (Right . Just <$> readTQueue exchange.replies)
          `orElse` ( readTVar exchange.finished >>= \case
                       True -> pure (Right Nothing)
                       False -> readTVar state.closed >>= maybe retry (pure . Left)
                   )
          `orElse` timer
  CloseExchange exchange -> atomically (forget state exchange)
  NewSubscriptionKey -> atomically $ do
    key <- readTVar state.nextSubscriptionKey
    writeTVar state.nextSubscriptionKey (key + 1)
    pure key

-- | Remove an exchange from the routing table and mark it as over.
forget :: MooState -> Exchange -> STM ()
forget state exchange = do
  modifyTVar' state.pending (Map.delete exchange.requestId)
  writeTVar exchange.finished True

-- | Route incoming frames until the connection closes or the server
-- breaks the protocol, then record why.
receiveLoop :: (WebSocket :> es, Concurrent :> es) => MooState -> Eff es ()
receiveLoop state =
  ( do
      failure <- Stream.fold (Fold.catMaybes Fold.one) (Stream.mapM (dispatch state) frames)
      closeConnection state (fromMaybe ConnectionLost failure)
  )
    `finally` closeConnection state ConnectionLost

-- | Record why no more reply will come. Keeps the first reason.
closeConnection :: Concurrent :> es => MooState -> RoonError -> Eff es ()
closeConnection state reason = atomically (modifyTVar' state.closed (Just . fromMaybe reason))

-- | Answer the requests of the server, in the order they came. If the
-- handler fails, the connection counts as lost: the server would no
-- longer get answers.
answerLoop :: (WebSocket :> es, Concurrent :> es) => MooState -> RequestHandler es -> Eff es ()
answerLoop state handler = forever answerNext `finally` closeConnection state ConnectionLost
  where
    answerNext = do
      message <- atomically (readTQueue state.serverRequests)
      (status, body) <- case splitServiceMethod message.name of
        Just (service, method) -> handler service method message.body
        Nothing -> pure ("InvalidRequest", JsonBody (object ["error" .= ("malformed request name: " <> message.name)]))
      void . sendFrame $
        encode MooMessage {verb = Complete, name = status, requestId = message.requestId, headers = Map.empty, body}

-- | Handle one frame. Returns the reason to stop, if any.
dispatch :: Concurrent :> es => MooState -> ByteString -> Eff es (Maybe RoonError)
dispatch state frame = case parse frame of
  Left err -> pure (Just (ProtocolViolation err))
  Right message ->
    Nothing <$ case message.verb of
      Request -> atomically (writeTQueue state.serverRequests message)
      Continue -> deliver message False
      Complete -> deliver message True
  where
    -- A reply to an unknown or closed exchange is dropped.
    deliver message final = atomically $ do
      exchanges <- readTVar state.pending
      case Map.lookup message.requestId exchanges of
        Nothing -> pure ()
        Just exchange -> do
          writeTQueue exchange.replies Reply {final, name = message.name, body = message.body}
          if final then forget state exchange else pure ()
