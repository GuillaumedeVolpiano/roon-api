-- | The real WebSocket transport, against a scripted server listening on
-- the loopback interface.
module Roon.Effect.WebSocketSpec (tests) where

import Control.Concurrent.Async (race_, wait, withAsync)
import Control.Concurrent.STM (atomically, readTQueue, writeTQueue)
import Control.Exception (bracket)
import Data.Aeson (Value (..), object, (.=))
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map, empty, singleton)
import Data.Text (Text, isPrefixOf, isSuffixOf)
import Data.Text.Encoding (encodeUtf8)
import Effectful (Eff, IOE, MonadIO (liftIO), runEff)
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Network.Socket (Family (AF_INET), SockAddr (SockAddrInet),
                       SocketType (Stream), bind, defaultProtocol, socketPort,
                       tupleToHostAddress)
import Network.Socket qualified as N (close, socket)
import Roon.Connection (Reconnect (..), withRoon, withRoonReconnecting)
import Roon.Effect.Moo (Reply (..), request)
import Roon.Effect.TokenStore (TokenStore, runTokenStorePure)
import Roon.Effect.WebSocket (ServerAddress (..), describeFrame, frames,
                              runWebSocketWith, sendFrame, traceFrames)
import Roon.Error (RoonError (..))
import Roon.Moo.Message (Body (JsonBody, NoBody, RawBody),
                         MooMessage (MooMessage, body, headers, name, requestId, verb),
                         RequestId (RequestId),
                         Verb (Complete, Continue, Request), encode)
import Roon.Registry (Registration (token))
import Roon.Test.Server (Wire (toClient, toServer), answer, closeConnection,
                         expectRequest, extension, field, newWire,
                         receiveFromClient, registered, sendToClient, serveInfo,
                         serverRequest)
import Streamly.Data.Fold (drainMapM)
import Streamly.Data.Stream (fold, toList)
import Streamly.External.Network.WebSocket.Server (Connection,
                                                   Message (BinaryMessage, TextMessage),
                                                   defaultServerOptions, listen,
                                                   messages, send, serveSocket)
import Streamly.External.Network.WebSocket.Server qualified as WSS (close)
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  localOption (mkTimeout 10_000_000) $
    testGroup
      "Roon.Effect.WebSocket"
      [ testGroup
          "tracing"
          [ testCase "describes each kind of frame" $ do
              let frame verb name requestId body = encode MooMessage {verb, name, requestId = RequestId requestId, headers = empty, body}
              describeFrame 20 "" @?= "empty frame"
              describeFrame 40 (frame Request "svc:1/get" "3" (JsonBody (object ["a" .= (1 :: Int)])))
                @?= "REQUEST svc:1/get [3] {\"a\":1}"
              describeFrame 10 (frame Complete "Success" "3" (JsonBody (object ["long" .= ("abcdefghij" :: Text)])))
                @?= "COMPLETE Success [3] {\"long\":\"a... (21 characters)"
              describeFrame 20 (frame Continue "Changed" "4" NoBody) @?= "CONTINUE Changed [4]"
              describeFrame 20 (frame Complete "Success" "5" (RawBody "image/jpeg" "\255\216\255"))
                @?= "COMPLETE Success [5] image/jpeg, 3 bytes"
              let unreadable = describeFrame 80 "HTTP/1.1 400 Bad Request\n\n"
              assertBool (show unreadable) $
                "not a MOO message (" `isPrefixOf` unreadable && "): HTTP/1.1 400 Bad Request\\n\\n" `isSuffixOf` unreadable
          , testCase "reports the frames of a connection" $ do
              lines' <- newIORef []
              let reply = encode MooMessage {verb = Complete, name = "Success", requestId = RequestId "0", headers = empty, body = NoBody}
              inbox <- newIORef [Just reply, Nothing]
              _ <- runEff . runWebSocketWith (\_ -> pure True) (liftIO (atomicModifyIORef' inbox (\case (x : rest) -> (rest, x); [] -> ([], Nothing)))) $
                traceFrames 80 (\line -> liftIO (modifyIORef' lines' (line :))) $
                  sendFrame "MOO/1 REQUEST svc:1/ask\nRequest-Id: 0\n\n" >> toList frames
              reverse <$> readIORef lines' >>= (@?= ["-> REQUEST svc:1/ask [0]", "<- COMPLETE Success [0]", "<- connection closed"])
          ]
      , testCase "registers over a real WebSocket" $ do
          (outcome, ()) <- withLoopbackServer empty (withRoon' pure) $ \wire -> do
            registerRequest <- serveInfo wire >> expectRequest wire "com.roonlabs.registry:1/register"
            sendToClient wire (serverRequest "0" "com.roonlabs.ping:1/ping")
            pong <- receiveFromClient wire
            (pong.verb, pong.name) @?= (Complete, "Success")
            answer wire registerRequest Continue "Registered" (registered "tok-1")
          fmap ((.token) . fst) outcome @?= Right "tok-1"
          fmap snd outcome @?= Right (singleton "core-1" "tok-1")
      , testCase "carries a body larger than one network packet" $ do
          let large = RawBody "application/octet-stream" (mconcat (replicate 20_000 "0123456789"))
              client address = withRoon address extension $ \_ -> request "svc:1/echo" large
          (outcome, ()) <- withLoopbackServer (singleton "core-1" "tok") client $ \wire -> do
            registerRequest <- serveInfo wire >> expectRequest wire "com.roonlabs.registry:1/register"
            answer wire registerRequest Continue "Registered" (registered "tok")
            echo <- expectRequest wire "svc:1/echo"
            answer wire echo Complete "Success" echo.body
          fmap ((.body) . fst) outcome @?= Right large
      , testCase "connects again after losing the connection" $ do
          let policy = Reconnect {firstDelay = 1_000, maxDelay = 10_000, maxAttempts = Just 5}
              client address = withRoonReconnecting policy address extension $ \_ -> (.name) <$> request "svc:1/echo" NoBody
          (outcome, ()) <- withLoopbackServer empty client $ \wire -> do
            -- The first connection is lost while the request waits.
            registerRequest <- serveInfo wire >> expectRequest wire "com.roonlabs.registry:1/register"
            answer wire registerRequest Continue "Registered" (registered "tok-1")
            _ <- expectRequest wire "svc:1/echo"
            closeConnection wire
            -- The second one answers.
            again <- serveInfo wire >> expectRequest wire "com.roonlabs.registry:1/register"
            field "token" again @?= Just (String "tok-1")
            answer wire again Continue "Registered" (registered "tok-1")
            echo <- expectRequest wire "svc:1/echo"
            answer wire echo Complete "Again" NoBody
          fmap fst outcome @?= Right "Again"
      , testCase "reports the loss when the server closes the connection" $ do
          (outcome, ()) <- withLoopbackServer empty (withRoon' pure) $ \wire -> do
            _ <- expectRequest wire "com.roonlabs.registry:1/info"
            closeConnection wire
          fmap fst outcome @?= Left ConnectionLost
      , testCase "reports a refused connection" $ do
          port <- freePort
          outcome <- runClient empty (withRoon' pure ServerAddress {host = "127.0.0.1", port})
          case outcome of
            Left (ConnectFailed _) -> pure ()
            other -> assertFailure ("unexpected outcome: " <> show (fmap fst other))
      ]
  where
    withRoon' action address = withRoon address extension action

type ClientEffects = '[TokenStore, Error RoonError, Concurrent, IOE]

runClient :: Map Text Text -> Eff ClientEffects a -> IO (Either RoonError (a, Map Text Text))
runClient tokens = runEff . runConcurrent . runErrorNoCallStack . runTokenStorePure tokens

-- | Run a client against a server script, over a WebSocket on a free
-- loopback port.
withLoopbackServer
  :: Map Text Text
  -> (ServerAddress -> Eff ClientEffects a)
  -> (Wire -> IO b)
  -> IO (Either RoonError (a, Map Text Text), b)
withLoopbackServer tokens client script = do
  wire <- newWire
  bracket (listen "127.0.0.1" 0) N.close $ \listener -> do
    port <- fromIntegral <$> socketPort listener
    withAsync (serveSocket listener defaultServerOptions (const (bridge wire))) $ \_ ->
      withAsync (script wire) $ \scriptThread -> do
        outcome <- runClient tokens (client ServerAddress {host = "127.0.0.1", port})
        result <- wait scriptThread
        pure (outcome, result)

-- | Serve one connection by moving frames between it and the wire.
bridge :: Wire -> Connection -> IO ()
bridge wire connection =
  race_
    (fold (drainMapM (atomically . writeTQueue wire.toServer . bytesOf)) (messages connection))
    forward
  where
    bytesOf = \case
      BinaryMessage bytes -> bytes
      TextMessage text -> encodeUtf8 text
    forward =
      atomically (readTQueue wire.toClient) >>= \case
        Just frame -> send connection (BinaryMessage frame) >> forward
        Nothing -> WSS.close connection

-- | A port that was free a moment ago.
freePort :: IO Int
freePort =
  bracket (N.socket AF_INET Stream defaultProtocol) N.close $ \socket -> do
    bind socket (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
    fromIntegral <$> socketPort socket
