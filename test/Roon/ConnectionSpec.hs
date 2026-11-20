-- | The connection logic, exercised against a scripted server over an
-- in-memory transport.
module Roon.ConnectionSpec (tests) where

import Data.Aeson (Value (..), object, (.=))
import Data.Map.Strict qualified as Map
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically, writeTQueue)
import Data.Text (Text)
import Effectful (liftIO)
import Effectful.Error.Static (runErrorNoCallStack)
import Roon.Connection (runRoon)
import Roon.Effect.Moo
import Roon.Error (RoonError (..))
import Roon.Moo.Message
import Roon.Registry
import Roon.Test.Server
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream qualified as Stream
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  localOption (mkTimeout 5_000_000) $
    testGroup
      "Roon.Connection"
      [ testCase "registers, answers a ping and stores the token" $ do
          (outcome, ()) <- withServer Map.empty (runRoon extension pure) $ \wire -> do
            registerRequest <- serveInfo wire >> expectRequest wire "com.roonlabs.registry:1/register"
            field "extension_id" registerRequest @?= Just (String "test.extension")
            field "token" registerRequest @?= Nothing
            field "provided_services" registerRequest @?= Just (toJSONList ["com.roonlabs.ping:1"])
            -- The server pings while the registration is pending.
            sendToClient wire (serverRequest "41" "com.roonlabs.ping:1/ping")
            pong <- receiveFromClient wire
            (pong.verb, pong.name, pong.requestId) @?= (Complete, "Success", RequestId "41")
            answer wire registerRequest Continue "Registered" (registered "tok-1")
          outcome
            @?= Right
              ( Registration
                  { core = testCore
                  , token = "tok-1"
                  , grantedServices = ["com.roonlabs.transport:2"]
                  }
              , Map.singleton "core-1" "tok-1"
              )
      , testCase "presents the stored token and replaces it" $ do
          (outcome, ()) <- withServer (Map.singleton "core-1" "old") (runRoon extension pure) $ \wire -> do
            registerRequest <- serveInfo wire >> expectRequest wire "com.roonlabs.registry:1/register"
            field "token" registerRequest @?= Just (String "old")
            answer wire registerRequest Continue "Registered" (registered "new")
          fmap snd outcome @?= Right (Map.singleton "core-1" "new")
      , testCase "rejects a request for a service it does not provide" $ do
          (outcome, ()) <- withServer Map.empty (runMoo defaultRequestHandler info) $ \wire -> do
            infoRequest <- expectRequest wire "com.roonlabs.registry:1/info"
            sendToClient wire (serverRequest "7" "com.example.unknown:1/anything")
            rejection <- receiveFromClient wire
            (rejection.verb, rejection.name, rejection.requestId) @?= (Complete, "InvalidRequest", RequestId "7")
            answer wire infoRequest Complete "Success" coreBody
          fmap fst outcome @?= Right testCore
      , testCase "routes replies while the server's request is being answered" $ do
          (gate, answered) <- (,) <$> newEmptyMVar <*> newEmptyMVar
          -- The answer to the server waits until the client has its reply.
          let handler service method body = case (service, method) of
                ("svc:1", "slow") -> liftIO (takeMVar gate) >> pure ("Success", NoBody)
                _ -> defaultRequestHandler service method body
              client = runMoo handler $ do
                reply <- request "svc:1/question" NoBody
                liftIO (putMVar gate ())
                -- Stay connected until the server has the answer.
                liftIO (takeMVar answered)
                pure reply.name
          (outcome, ()) <- withServer Map.empty client $ \wire -> do
            question <- expectRequest wire "svc:1/question"
            sendToClient wire (serverRequest "9" "svc:1/slow")
            answer wire question Complete "Answer" NoBody
            slow <- receiveFromClient wire
            (slow.verb, slow.name, slow.requestId) @?= (Complete, "Success", RequestId "9")
            putMVar answered ()
          fmap fst outcome @?= Right "Answer"
      , testCase "routes replies to the exchange they answer" $ do
          let client = runMoo defaultRequestHandler $ do
                first <- openExchange "svc:1/first" NoBody
                second <- openExchange "svc:1/second" NoBody
                (,) <$> nextReply first <*> nextReply second
          (outcome, ()) <- withServer Map.empty client $ \wire -> do
            first <- expectRequest wire "svc:1/first"
            second <- expectRequest wire "svc:1/second"
            answer wire second Complete "Second" NoBody
            answer wire first Complete "First" NoBody
          fmap (both (fmap (.name)) . fst) outcome @?= Right (Just "First", Just "Second")
      , testCase "streams the replies of a subscription until the final one" $ do
          let client =
                runMoo defaultRequestHandler $
                  withSubscription "svc:1/subscribe_things" NoBody (Stream.fold Fold.toList . fmap (.name))
          (outcome, ()) <- withServer Map.empty client $ \wire -> do
            subscription <- expectRequest wire "svc:1/subscribe_things"
            answer wire subscription Continue "Subscribed" NoBody
            answer wire subscription Continue "Changed" NoBody
            answer wire subscription Complete "Unsubscribed" NoBody
          fmap fst outcome @?= Right ["Subscribed", "Changed", "Unsubscribed"]
      , testCase "fails pending requests when the connection closes" $ do
          (outcome, ()) <- withServer Map.empty (runMoo defaultRequestHandler info) $ \wire -> do
            _ <- expectRequest wire "com.roonlabs.registry:1/info"
            closeConnection wire
          fmap fst outcome @?= Left ConnectionLost
      , testCase "refuses new requests once the connection is closed" $ do
          -- Both failures reach the handler of the caller, not the one
          -- around runMoo.
          let client = runMoo defaultRequestHandler $ do
                first <- runErrorNoCallStack @RoonError info
                second <- runErrorNoCallStack @RoonError info
                pure (first, second)
          (outcome, ()) <- withServer Map.empty client $ \wire -> do
            _ <- expectRequest wire "com.roonlabs.registry:1/info"
            closeConnection wire
          fmap fst outcome @?= Right (Left ConnectionLost, Left ConnectionLost)
      , testCase "reports a frame that is not a MOO message" $ do
          (outcome, ()) <- withServer Map.empty (runMoo defaultRequestHandler info) $ \wire -> do
            _ <- expectRequest wire "com.roonlabs.registry:1/info"
            atomically (writeTQueue wire.toClient (Just "HTTP/1.1 400 Bad Request\n\n"))
          case fmap fst outcome of
            Left (ProtocolViolation _) -> pure ()
            other -> assertFailure ("unexpected outcome: " <> show other)
      , testCase "gives up on a request that gets no reply in time" $ do
          let client = runMoo defaultRequestHandler $ do
                late <- runErrorNoCallStack @RoonError (requestWithin (Just 50_000) "svc:1/slow" NoBody)
                next <- request "svc:1/fast" NoBody
                pure (late, next.name)
          (outcome, ()) <- withServer Map.empty client $ \wire -> do
            slow <- expectRequest wire "svc:1/slow"
            fast <- expectRequest wire "svc:1/fast"
            -- Too late: the client has stopped waiting, and drops it.
            answer wire slow Complete "Late" NoBody
            answer wire fast Complete "Fast" NoBody
          fmap fst outcome @?= Right (Left (TimedOut "svc:1/slow"), "Fast")
      , testCase "gives up on a registration that is not enabled in time" $ do
          let client = runMoo defaultRequestHandler (registerWithin 50_000 extension Nothing)
          (outcome, ()) <- withServer Map.empty client $ \wire -> do
            _ <- expectRequest wire "com.roonlabs.registry:1/register"
            pure ()
          fmap fst outcome @?= Left (TimedOut "com.roonlabs.registry:1/register")
      , testCase "reports an unexpected status" $ do
          (outcome, ()) <- withServer Map.empty (runMoo defaultRequestHandler info) $ \wire -> do
            infoRequest <- expectRequest wire "com.roonlabs.registry:1/info"
            answer wire infoRequest Complete "NotAvailable" NoBody
          fmap fst outcome @?= Left (UnexpectedReply "com.roonlabs.registry:1/info" "NotAvailable")
      , testCase "reports an unexpected body" $ do
          (outcome, ()) <- withServer Map.empty (runMoo defaultRequestHandler info) $ \wire -> do
            infoRequest <- expectRequest wire "com.roonlabs.registry:1/info"
            answer wire infoRequest Complete "Success" (JsonBody (object ["core_id" .= ("core-1" :: Text)]))
          case fmap fst outcome of
            Left (UnexpectedReplyBody "com.roonlabs.registry:1/info" _) -> pure ()
            other -> assertFailure ("unexpected outcome: " <> show other)
      ]
  where
    both f (a, b) = (f a, f b)
    toJSONList = Array . foldMap (pure . String)
