-- |
-- Module      : Roon.Service
-- Description : Building blocks for the services of a Roon Server
-- License     : BSD-3-Clause
--
-- What the service modules share: checking the status of a reply,
-- decoding its body, and the subscription convention of Roon services.
module Roon.Service
  ( -- * Replies
    expect
  , expectSuccess
  , call

    -- * Subscriptions
  , subscribe
  , subscriptionUpdates
  ) where

import Data.Aeson (FromJSON (..), Value (..), object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair, parseEither)
import Data.Text (Text)
import Data.Text qualified as Text
import Effectful
import Effectful.Error.Static (Error, catchError, throwError)
import Effectful.Exception (finally)
import Roon.Effect.Moo (Moo, Reply (..), closeExchange, newSubscriptionKey, nextReply, openExchange, request)
import Roon.Error (RoonError (..))
import Roon.Moo.Message (Body (..))
import Streamly.Data.Stream (Stream)
import Streamly.Data.Stream qualified as Stream

-- | Check the status of a reply and decode its JSON body. Takes the
-- request name, for the error, and the expected status.
expect :: (FromJSON a, Error RoonError :> es) => Text -> Text -> Reply -> Eff es a
expect requestName status reply
  | reply.name /= status = throwError (UnexpectedReply requestName reply.name)
  | otherwise = case reply.body of
      JsonBody value -> decodeBody requestName value
      _ -> throwError (UnexpectedReplyBody requestName "not a JSON body")

-- | Check that a reply says @Success@, whatever its body.
expectSuccess :: Error RoonError :> es => Text -> Reply -> Eff es ()
expectSuccess requestName reply
  | reply.name /= "Success" = throwError (UnexpectedReply requestName reply.name)
  | otherwise = pure ()

-- | Send a request that only answers @Success@ or a failure.
call :: (Moo :> es, Error RoonError :> es) => Text -> Value -> Eff es ()
call name body = request name (JsonBody body) >>= expectSuccess name

decodeBody :: (FromJSON a, Error RoonError :> es) => Text -> Value -> Eff es a
decodeBody requestName = either (throwError . UnexpectedReplyBody requestName . Text.pack) pure . parseEither parseJSON

-- | Subscribe to a topic of a service, such as the zones of
-- @com.roonlabs.transport:2@, and consume the replies.
--
-- The request is @service/subscribe_topic@, its body the arguments with a
-- @subscription_key@ added. The server answers @Subscribed@, then
-- @Changed@ as often as needed, and @Unsubscribed@ as the final reply
-- once the subscription ends. When the consumer returns, this sends
-- @service/unsubscribe_topic@ with the same key, without waiting for the
-- answer; a connection already lost is not an error at that point.
subscribe
  :: (Moo :> es, Error RoonError :> es)
  => Text
  -- ^ The service.
  -> Text
  -- ^ The topic.
  -> [Pair]
  -- ^ The arguments.
  -> (Stream (Eff es) Reply -> Eff es a)
  -> Eff es a
subscribe service topic arguments consume = do
  key <- newSubscriptionKey
  let withKey = object (("subscription_key" .= key) : arguments)
  exchange <- openExchange (service <> "/subscribe_" <> topic) (JsonBody withKey)
  consume (Stream.unfoldrM (const (fmap (,()) <$> nextReply exchange)) ())
    `finally` (unsubscribe key >> closeExchange exchange)
  where
    unsubscribe key =
      ( do
          stop <- openExchange (service <> "/unsubscribe_" <> topic) (JsonBody (object ["subscription_key" .= key]))
          closeExchange stop
      )
        `catchError` \_ (_ :: RoonError) -> pure ()

-- | Decode the replies of a subscription. The first argument decodes the
-- body of @Subscribed@, the second that of @Changed@. The stream ends with
-- @Unsubscribed@, the final reply; any other status is an error.
subscriptionUpdates
  :: (FromJSON initial, FromJSON change, Error RoonError :> es)
  => Text
  -- ^ The request name, for errors.
  -> (initial -> update)
  -> (change -> update)
  -> Stream (Eff es) Reply
  -> Stream (Eff es) update
subscriptionUpdates requestName fromInitial fromChange = Stream.mapMaybeM decode
  where
    decode reply = case reply.name of
      "Subscribed" -> Just . fromInitial <$> replyBody requestName reply
      "Changed" -> Just . fromChange <$> replyBody requestName reply
      "Unsubscribed" -> pure Nothing
      other -> throwError (UnexpectedReply requestName other)

-- | Decode the body of a reply; a missing body reads as an empty object.
replyBody :: (FromJSON a, Error RoonError :> es) => Text -> Reply -> Eff es a
replyBody requestName reply = case reply.body of
  JsonBody value -> decodeBody requestName value
  NoBody -> decodeBody requestName (Object KeyMap.empty)
  _ -> throwError (UnexpectedReplyBody requestName "not a JSON body")
