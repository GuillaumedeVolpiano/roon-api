module Roon.Effect.TokenStoreSpec (tests) where

import Data.Bits ((.&.))
import Data.Map.Strict (fromList, singleton)
import Effectful (runEff, runPureEff)
import Effectful.FileSystem (runFileSystem)
import Roon.Effect.TokenStore (loadToken, runTokenStoreFile, runTokenStorePure,
                               saveToken)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (fileMode, getFileStatus)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Roon.Effect.TokenStore"
    [ testCase "the pure store returns what was saved" $ do
        let (loaded, final) = runPureEff . runTokenStorePure (singleton "a" "1") $ do
              saveToken "b" "2"
              (,,) <$> loadToken "a" <*> loadToken "b" <*> loadToken "c"
        loaded @?= (Just "1", Just "2", Nothing)
        final @?= fromList [("a", "1"), ("b", "2")]
    , testCase "the file store has no token before the file exists" $
        withSystemTempDirectory "roon-api-test" $ \directory -> do
          loaded <- runEff . runFileSystem . runTokenStoreFile (directory </> "missing" </> "tokens.json") $ loadToken "a"
          loaded @?= Nothing
    , testCase "the file store keeps tokens across runs, one per core" $
        withSystemTempDirectory "roon-api-test" $ \directory -> do
          let path = directory </> "nested" </> "tokens.json"
          runEff . runFileSystem . runTokenStoreFile path $ saveToken "a" "1" >> saveToken "b" "2" >> saveToken "a" "3"
          loaded <- runEff . runFileSystem . runTokenStoreFile path $ (,,) <$> loadToken "a" <*> loadToken "b" <*> loadToken "c"
          loaded @?= (Just "3", Just "2", Nothing)
    , testCase "the file is private and no temporary file is left behind" $
        withSystemTempDirectory "roon-api-test" $ \directory -> do
          let path = directory </> "tokens.json"
          runEff . runFileSystem . runTokenStoreFile path $ saveToken "a" "1" >> saveToken "a" "2"
          status <- getFileStatus path
          fileMode status .&. 0o077 @?= 0
          listDirectory directory >>= (@?= ["tokens.json"])
    , testCase "an unreadable file counts as empty and is replaced" $
        withSystemTempDirectory "roon-api-test" $ \directory -> do
          let path = directory </> "tokens.json"
          writeFile path "not json"
          loaded <- runEff . runFileSystem . runTokenStoreFile path $ loadToken "a" <* saveToken "a" "1"
          loaded @?= Nothing
          reloaded <- runEff . runFileSystem . runTokenStoreFile path $ loadToken "a"
          reloaded @?= Just "1"
    ]
