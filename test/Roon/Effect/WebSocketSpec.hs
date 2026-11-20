-- | The real WebSocket transport, against a scripted server listening on
-- the loopback interface.
module Roon.Effect.WebSocketSpec (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_, wait, withAsync)
import Control.Concurrent.STM (atomically, readTQueue, writeTQueue)
import Control.Exception (bracket)
import Control.Monad (forever)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Aeson (Value (..))
import Data.Text (Text)
import Effectful
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Network.Socket qualified as Socket
import Network.WebSockets qualified as WS
import Roon.Connection (Reconnect (..), withRoon, withRoonReconnecting)
import Roon.Effect.Moo (Reply (..), request)
import Roon.Effect.TokenStore (TokenStore, runTokenStorePure)
import Roon.Effect.WebSocket (ServerAddress (..))
import Roon.Error (RoonError (..))
import Roon.Moo.Message
import Roon.Registry
import Roon.Test.Server
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  localOption (mkTimeout 10_000_000) $
    testGroup
      "Roon.Effect.WebSocket"
      [ testCase "registers over a real WebSocket" $ do
          (outcome, ()) <- withLoopbackServer Map.empty (withRoon' pure) $ \wire -> do
            registerRequest <- serveInfo wire >> expectRequest wire "com.roonlabs.registry:1/register"
            sendToClient wire (serverRequest "0" "com.roonlabs.ping:1/ping")
            pong <- receiveFromClient wire
            (pong.verb, pong.name) @?= (Complete, "Success")
            answer wire registerRequest Continue "Registered" (registered "tok-1")
          fmap ((.token) . fst) outcome @?= Right "tok-1"
          fmap snd outcome @?= Right (Map.singleton "core-1" "tok-1")
      , testCase "carries a body larger than one network packet" $ do
          let large = RawBody "application/octet-stream" (mconcat (replicate 20_000 "0123456789"))
              client address = withRoon address extension $ \_ -> request "svc:1/echo" large
          (outcome, ()) <- withLoopbackServer (Map.singleton "core-1" "tok") client $ \wire -> do
            registerRequest <- serveInfo wire >> expectRequest wire "com.roonlabs.registry:1/register"
            answer wire registerRequest Continue "Registered" (registered "tok")
            echo <- expectRequest wire "svc:1/echo"
            answer wire echo Complete "Success" echo.body
          fmap ((.body) . fst) outcome @?= Right large
      , testCase "connects again after losing the connection" $ do
          let policy = Reconnect {firstDelay = 1_000, maxDelay = 10_000, maxAttempts = Just 5}
              client address = withRoonReconnecting policy address extension $ \_ -> (.name) <$> request "svc:1/echo" NoBody
          (outcome, ()) <- withLoopbackServer Map.empty client $ \wire -> do
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
          (outcome, ()) <- withLoopbackServer Map.empty (withRoon' pure) $ \wire -> do
            _ <- expectRequest wire "com.roonlabs.registry:1/info"
            closeConnection wire
          fmap fst outcome @?= Left ConnectionLost
      , testCase "reports a refused connection" $ do
          port <- freePort
          outcome <- runClient Map.empty (withRoon' pure ServerAddress {host = "127.0.0.1", port})
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
  port <- freePort
  withAsync (WS.runServer "127.0.0.1" port (bridge wire)) $ \_ ->
    withAsync (script wire) $ \scriptThread -> do
      outcome <- connect (50 :: Int) ServerAddress {host = "127.0.0.1", port}
      result <- wait scriptThread
      pure (outcome, result)
  where
    -- The server may not be listening yet: retry refused connections.
    connect attempts address =
      runClient tokens (client address) >>= \case
        Left (ConnectFailed _) | attempts > 0 -> threadDelay 20_000 >> connect (attempts - 1) address
        outcome -> pure outcome

-- | Serve one connection by moving frames between it and the wire.
bridge :: Wire -> WS.ServerApp
bridge wire pending = do
  connection <- WS.acceptRequest pending
  race_
    (forever (WS.receiveData connection >>= atomically . writeTQueue wire.toServer))
    (forward connection)
  where
    forward connection =
      atomically (readTQueue wire.toClient) >>= \case
        Just frame -> WS.sendBinaryData connection frame >> forward connection
        Nothing -> WS.sendClose connection ("" :: ByteString)

-- | A port that was free a moment ago.
freePort :: IO Int
freePort =
  bracket (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol) Socket.close $ \socket -> do
    Socket.bind socket (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
    fromIntegral <$> Socket.socketPort socket
