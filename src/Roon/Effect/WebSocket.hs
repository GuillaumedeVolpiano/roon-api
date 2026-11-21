-- |
-- Module      : Roon.Effect.WebSocket
-- Description : The frame transport under the MOO protocol
-- License     : BSD-3-Clause
--
-- A Roon Server exposes its extension API on @ws:\/\/host:port\/api@ and
-- exchanges binary frames there. This module adapts the 'WebSocket' effect
-- of @effectful-websockets@ to that use.
module Roon.Effect.WebSocket
  ( -- * Effect
    WebSocket
  , sendFrame
  , frames

    -- * Interpreters
  , ServerAddress (..)
  , localServer
  , runWebSocket
  , runWebSocketWith
  ) where

import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)
import Effectful
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.WebSocket (Message (..), WebSocket, messages, sendBinary)
import Effectful.WebSocket qualified as WebSocket
import Effectful.WebSocket.Client (WebSocketError, clientConfig, runWebSocketClient)
import Effectful.WebSocket.Client qualified as Client
import Roon.Error (RoonError (..))
import Streamly.Data.Stream (Stream)

-- | Send one frame. 'False' means the connection is closed and the frame
-- was not sent.
sendFrame :: WebSocket :> es => ByteString -> Eff es Bool
sendFrame = sendBinary

-- | The frames received until the connection closes. Roon sends binary
-- frames; a text frame is passed on as its UTF-8 bytes.
frames :: WebSocket :> es => Stream (Eff es) ByteString
frames = fmap toBytes messages
  where
    toBytes = \case
      BinaryMessage bytes -> bytes
      TextMessage text -> encodeUtf8 text

-- | Where a Roon Server listens.
data ServerAddress = ServerAddress
  { host :: String
  , port :: Int
  }
  deriving stock (Eq, Show)

-- | A Roon Server on this machine, on its default port.
localServer :: ServerAddress
localServer = ServerAddress {host = "127.0.0.1", port = 9330}

-- | Interpret the effect with two actions on frames. Meant for tests.
runWebSocketWith
  :: (ByteString -> Eff es Bool)
  -- ^ Send a frame; 'False' when the connection is closed.
  -> Eff es (Maybe ByteString)
  -- ^ Receive a frame; 'Nothing' when the connection is closed.
  -> Eff (WebSocket : es) a
  -> Eff es a
runWebSocketWith sendAction receiveAction =
  WebSocket.runWebSocketWith
    (sendAction . \case BinaryMessage bytes -> bytes; TextMessage text -> encodeUtf8 text)
    (fmap BinaryMessage <$> receiveAction)

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
runWebSocket address action =
  runErrorNoCallStack @WebSocketError (runWebSocketClient config (inject action)) >>= \case
    Right value -> pure value
    Left (Client.ConnectFailed cause) -> throwError (ConnectFailed cause)
    Left Client.NoPong -> throwError ConnectionLost
  where
    -- Roon Server 2.75 never answers the close frame: waiting for it would
    -- last the whole limit. A short wait still lets Roon read our last
    -- frames, and us read its own, before the TCP connection goes.
    config = (clientConfig address.host address.port "/api") {Client.closeTimeout = 100_000}
