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

    -- * Tracing
  , traceFrames
  , describeFrame

    -- * Interpreters
  , ServerAddress (..)
  , localServer
  , runWebSocket
  , runWebSocketWith
  ) where

import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Text.Encoding (encodeUtf8)
import Effectful
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.WebSocket (Message (..), Traffic (..), WebSocket, messages, sendBinary, traced)
import Effectful.WebSocket qualified as WebSocket
import Effectful.WebSocket.Client (WebSocketError, clientConfig, runWebSocketClient)
import Effectful.WebSocket.Client qualified as Client
import Roon.Error (RoonError (..))
import Roon.Moo.Message (Body (..), MooMessage (..), RequestId (..), parse)
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

-- | Report every frame of an action, as one line of text each: @->@ for
-- the frames sent, @<-@ for those received. Bodies are cut after the given
-- number of characters.
traceFrames :: WebSocket :> es => Int -> (Text -> Eff es ()) -> Eff es a -> Eff es a
traceFrames limit report = traced $ \case
  Sent message sent -> report ("-> " <> describeMessage message <> (if sent then "" else " (not sent: connection closed)"))
  Received (Just message) -> report ("<- " <> describeMessage message)
  Received Nothing -> report "<- connection closed"
  where
    describeMessage = \case
      BinaryMessage bytes -> describeFrame limit bytes
      TextMessage text -> "text frame, not MOO: " <> cut limit text

-- | One line about a frame: its verb, name, request id and body, cut after
-- the given number of characters, or why it is not a MOO message.
describeFrame :: Int -> ByteString -> Text
describeFrame limit frame
  | ByteString.null frame = "empty frame"
  | otherwise = case parse frame of
      Left err -> "not a MOO message (" <> Text.pack (show err) <> "): " <> cut limit (decodeUtf8Lenient frame)
      Right MooMessage {verb, name, requestId = RequestId requestId, body} ->
        Text.toUpper (Text.pack (show verb)) <> " " <> name <> " [" <> requestId <> "]" <> case body of
          NoBody -> ""
          JsonBody value -> " " <> cut limit (decodeUtf8Lenient (LazyByteString.toStrict (Aeson.encode value)))
          RawBody contentType bytes -> " " <> contentType <> ", " <> Text.pack (show (ByteString.length bytes)) <> " bytes"

cut :: Int -> Text -> Text
cut limit text
  | Text.length flat <= limit = flat
  | otherwise = Text.take limit flat <> "... (" <> Text.pack (show (Text.length flat)) <> " characters)"
  where
    flat = Text.replace "\n" "\\n" text
