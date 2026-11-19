-- |
-- Module      : Roon.Effect.TokenStore
-- Description : Where the authorisation tokens are kept
-- License     : BSD-3-Clause
--
-- When the user enables an extension in Roon, the server hands it a
-- token. Presenting that token on later connections skips the manual
-- step. There is one token per server, identified by its core id.
module Roon.Effect.TokenStore
  ( -- * Effect
    TokenStore (..)
  , loadToken
  , saveToken

    -- * Interpreters
  , runTokenStoreFile
  , defaultTokenFile
  , runTokenStorePure
  ) where

import Control.Exception (bracketOnError)
import Data.Aeson ((.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Effectful
import Effectful.Dispatch.Dynamic (interpret_, reinterpret_, send)
import Effectful.State.Static.Local (gets, modify, runState)
import System.Directory
  ( XdgDirectory (XdgState)
  , createDirectoryIfMissing
  , doesFileExist
  , getXdgDirectory
  , removeFile
  , renameFile
  )
import System.FilePath (takeDirectory, (<.>), (</>))
import System.IO (hClose, openBinaryTempFile)

-- | Remember the token that each server gave to this extension.
data TokenStore :: Effect where
  LoadToken :: Text -> TokenStore m (Maybe Text)
  SaveToken :: Text -> Text -> TokenStore m ()

type instance DispatchOf TokenStore = Dynamic

-- | The token for the server with this core id, if one was saved.
loadToken :: TokenStore :> es => Text -> Eff es (Maybe Text)
loadToken = send . LoadToken

-- | Save the token for the server with this core id.
saveToken :: TokenStore :> es => Text -> Text -> Eff es ()
saveToken coreId token = send (SaveToken coreId token)

-- | Keep the tokens in a JSON file.
--
-- The file is created readable by its owner only, and replaced
-- atomically. A missing or unreadable file counts as empty: the cost is
-- to enable the extension in Roon once more.
runTokenStoreFile :: IOE :> es => FilePath -> Eff (TokenStore : es) a -> Eff es a
runTokenStoreFile path = interpret_ $ \case
  LoadToken coreId -> liftIO (Map.lookup coreId <$> readTokens path)
  SaveToken coreId token -> liftIO $ do
    tokens <- readTokens path
    writeTokens path (Map.insert coreId token tokens)

-- | The conventional place for the tokens of an extension:
-- @$XDG_STATE_HOME\/roon-api\/\<extension id\>.json@.
defaultTokenFile :: Text -> IO FilePath
defaultTokenFile extensionId = do
  directory <- getXdgDirectory XdgState "roon-api"
  pure (directory </> Text.unpack extensionId <.> "json")

-- | Keep the tokens in memory, starting from the given ones. Returns the
-- tokens held at the end. Meant for tests.
runTokenStorePure :: Map Text Text -> Eff (TokenStore : es) a -> Eff es (a, Map Text Text)
runTokenStorePure initial = reinterpret_ (runState initial) $ \case
  LoadToken coreId -> gets (Map.lookup coreId)
  SaveToken coreId token -> modify (Map.insert coreId token)

readTokens :: FilePath -> IO (Map Text Text)
readTokens path = do
  exists <- doesFileExist path
  if not exists
    then pure Map.empty
    else do
      contents <- BS.readFile path
      pure . maybe Map.empty id $
        Aeson.decodeStrict contents >>= Aeson.parseMaybe (Aeson.withObject "token file" (.: "tokens"))

writeTokens :: FilePath -> Map Text Text -> IO ()
writeTokens path tokens = do
  let directory = takeDirectory path
  createDirectoryIfMissing True directory
  -- 'openBinaryTempFile' creates the file with mode 0600.
  bracketOnError
    (openBinaryTempFile directory "tokens.tmp")
    (\(temporary, handle) -> hClose handle >> removeFile temporary)
    ( \(temporary, handle) -> do
        BSL.hPut handle (Aeson.encode (Aeson.object ["tokens" .= tokens]))
        hClose handle
        renameFile temporary path
    )
