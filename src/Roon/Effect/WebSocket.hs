-- |
-- Module      : Roon.Effect.WebSocket
-- Description : The frame transport under the MOO protocol
-- License     : BSD-3-Clause
--
-- A Roon Server exposes its extension API on @ws:\/\/host:port\/api@. This
-- effect is the little the rest of the library needs from that socket:
-- sending and receiving whole binary frames.
module Roon.Effect.WebSocket
  ( -- * Effect
    WebSocket (..)
  , sendFrame
  , receiveFrame
  , frames

    -- * Interpreters
  , ServerAddress (..)
  , localServer
  , runWebSocket
  , runWebSocketWith
  ) where

import Control.Concurrent.Async (ExceptionInLinkedThread (..), link, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, SomeException, catch, displayException, finally, fromException, throwIO, try)
import Data.ByteString (ByteString)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text qualified as Text
import Effectful
import Effectful.Dispatch.Dynamic (interpret_, send)
import Effectful.Error.Static (Error, throwError)
import Network.WebSockets qualified as WS
import Network.WebSockets.Connection.PingPong (PongTimeout (..))
import Roon.Error (RoonError (..))
import Streamly.Data.Stream (Stream)
import Streamly.Data.Stream qualified as Stream

-- | Exchange binary frames with the server.
data WebSocket :: Effect where
  SendFrame :: ByteString -> WebSocket m Bool
  ReceiveFrame :: WebSocket m (Maybe ByteString)

type instance DispatchOf WebSocket = Dynamic

-- | Send one frame. 'False' means the connection is closed and the frame
-- was not sent.
sendFrame :: WebSocket :> es => ByteString -> Eff es Bool
sendFrame = send . SendFrame

-- | Wait for the next frame. 'Nothing' means the connection is closed.
receiveFrame :: WebSocket :> es => Eff es (Maybe ByteString)
receiveFrame = send ReceiveFrame

-- | The frames received until the connection closes.
frames :: WebSocket :> es => Stream (Eff es) ByteString
frames = Stream.unfoldrM (const (fmap (,()) <$> receiveFrame)) ()

-- | Where a Roon Server listens.
data ServerAddress = ServerAddress
  { host :: String
  , port :: Int
  }
  deriving stock (Eq, Show)

-- | A Roon Server on this machine, on its default port.
localServer :: ServerAddress
localServer = ServerAddress {host = "127.0.0.1", port = 9330}

-- | Interpret the effect with two actions. Meant for tests and for other
-- transports.
runWebSocketWith
  :: (ByteString -> Eff es Bool)
  -- ^ Send a frame; 'False' when the connection is closed.
  -> Eff es (Maybe ByteString)
  -- ^ Receive a frame; 'Nothing' when the connection is closed.
  -> Eff (WebSocket : es) a
  -> Eff es a
runWebSocketWith sendAction receiveAction = interpret_ $ \case
  SendFrame frame -> sendAction frame
  ReceiveFrame -> receiveAction

-- | Open a connection to a Roon Server and run an action over it.
--
-- The server is pinged every ten seconds, as the reference implementation
-- does. Throws 'ConnectFailed' if the connection cannot be opened, and
-- 'ConnectionLost' if the server stops answering pings before the action
-- is done. The action must keep receiving frames for pongs to be seen.
runWebSocket
  :: (IOE :> es, Error RoonError :> es)
  => ServerAddress
  -> Eff (WebSocket : es) a
  -> Eff es a
runWebSocket address action = do
  outcome <- withEffToIO SeqUnlift $ \runInIO -> do
    connected <- newIORef False
    attempt <- try $ WS.runClient address.host address.port "/api" $ \connection -> do
      writeIORef connected True
      finished <- newEmptyMVar
      -- 'WS.withPingPong' runs its argument in a thread of its own. The
      -- action must stay in this thread, or the changes it makes to
      -- thread-local effects such as @State@ would be lost. So the
      -- watchdog only waits for the action to finish, and is linked to
      -- this thread to interrupt it on a missed pong.
      withAsync (WS.withPingPong pingPongOptions connection (const (takeMVar finished))) $ \watchdog -> do
        link watchdog
        value <-
          runInIO (runWebSocketWith (liftIO . sendTo connection) (liftIO (receiveFrom connection)) action)
            `finally` putMVar finished ()
        WS.sendClose connection ("" :: ByteString) `catch` \(_ :: SomeException) -> pure ()
        pure value
    wasConnected <- readIORef connected
    case attempt of
      Right value -> pure (Right value)
      Left (err :: SomeException)
        | not wasConnected -> pure (Left (ConnectFailed (Text.pack (displayException err))))
        | Just (ExceptionInLinkedThread _ cause) <- fromException err
        , Just PongTimeout <- fromException cause ->
            pure (Left ConnectionLost)
        | otherwise -> throwIO err
  either throwError pure outcome
  where
    pingPongOptions = WS.defaultPingPongOptions {WS.pingInterval = 10, WS.pongTimeout = 20}

sendTo :: WS.Connection -> ByteString -> IO Bool
sendTo connection frame =
  (True <$ WS.sendBinaryData connection frame)
    `catch` (\(_ :: WS.ConnectionException) -> pure False)
    `catch` (\(_ :: IOException) -> pure False)

receiveFrom :: WS.Connection -> IO (Maybe ByteString)
receiveFrom connection =
  (Just <$> WS.receiveData connection)
    `catch` (\(_ :: WS.ConnectionException) -> pure Nothing)
    `catch` (\(_ :: IOException) -> pure Nothing)
