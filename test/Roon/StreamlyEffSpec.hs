-- | Pins down how the concurrent combinators of streamly behave inside 'Eff'.
--
-- streamly runs every concurrent stream in a worker thread and reaches the
-- base monad through 'MonadBaseControl', which 'Eff' implements with the
-- unlifting strategy of the 'IOE' context. These tests record what that
-- means for the library: which strategy is needed, what happens to local and
-- shared state, and whether errors and exceptions reach the consumer.
module Roon.StreamlyEffSpec (tests) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (ErrorCall (..), SomeException, throwIO, try)
import Data.List (sort)
import Effectful
import Effectful.Error.Static (runErrorNoCallStack, throwError)
import Effectful.Reader.Static (ask, runReader)
import Effectful.State.Static.Local qualified as Local
import Effectful.State.Static.Shared qualified as Shared
import Streamly.Data.Stream.Prelude (Stream)
import Streamly.Data.Stream.Prelude qualified as Stream
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "streamly in Eff"
    [ testCase "the default sequential strategy fails in worker threads" $ do
        outcome <- try @SomeException . runEff $ Stream.toList (Stream.parList id [effect 1, effect 2])
        case outcome of
          Left _ -> pure ()
          Right values -> assertFailure ("expected a failure, got " <> show values)
    , testCase "with a concurrent strategy, parList merges every stream" $ do
        values <- runEff . concurrently $ Stream.toList (Stream.parList id (map effect [1 .. 50]))
        sort values @?= [1 .. 50]
    , testCase "the streams really run at the same time" $ do
        -- Each stream waits for the other: a sequential evaluation would block.
        (left, right) <- (,) <$> newEmptyMVar <*> newEmptyMVar
        let ping = Stream.fromEffect (liftIO (putMVar left ()) >> liftIO (takeMVar right) >> pure "ping")
            pong = Stream.fromEffect (liftIO (takeMVar left) >> liftIO (putMVar right ()) >> pure "pong")
        values <- runEff . concurrently $ Stream.toList (Stream.parList id [ping, pong :: Stream (Eff '[IOE]) String])
        sort values @?= ["ping", "pong"]
    , testCase "workers see the static environment (Reader)" $ do
        values <- runEff . runReader (10 :: Int) . concurrently $
          Stream.toList (Stream.parMapM id (\n -> (+ n) <$> ask @Int) (Stream.fromList [1, 2, 3]))
        sort values @?= [11, 12, 13]
    , testCase "local state changed in a worker stays in that worker" $ do
        (_, final) <- runEff . Local.runState (0 :: Int) . concurrently $
          Stream.toList (Stream.parList id (replicate 3 (Stream.fromEffect (Local.modify @Int (+ 1)))))
        final @?= 0
    , testCase "shared state changed in a worker reaches the caller" $ do
        (_, final) <- runEff . Shared.runState (0 :: Int) . concurrently $
          Stream.toList (Stream.parList id (replicate 3 (Stream.fromEffect (Shared.modify @Int (+ 1)))))
        final @?= 3
    , testCase "an Error thrown in a worker reaches the handler of the caller" $ do
        outcome <- runEff . runErrorNoCallStack @String . concurrently $
          Stream.toList (Stream.parList id [effect 1, Stream.fromEffect (throwError @String "boom"), effect 3])
        outcome @?= Left "boom"
    , testCase "an IO exception thrown in a worker reaches the consumer" $ do
        outcome <- try @ErrorCall . runEff . concurrently $
          Stream.toList (Stream.parList id [effect 1, Stream.fromEffect (liftIO (throwIO (ErrorCall "boom")))])
        outcome @?= Left (ErrorCall "boom")
    ]
  where
    effect :: Int -> Stream (Eff es) Int
    effect = Stream.fromEffect . pure

-- | The strategy streamly needs: every worker thread gets its own copy of
-- the environment.
concurrently :: IOE :> es => Eff es a -> Eff es a
concurrently = withUnliftStrategy (ConcUnlift Ephemeral Unlimited)
