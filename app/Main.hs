-- | @roon-probe@: connects to a Roon Server, registers as an extension and
-- prints what the server answers. A smoke test for the library against a
-- real server.
module Main (main) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.Error.Static (runErrorNoCallStack)
import Roon.Effect.Moo (defaultRequestHandler, pingService, runMoo)
import Roon.Effect.TokenStore (defaultTokenFile, loadToken, runTokenStoreFile, saveToken)
import Roon.Effect.WebSocket (ServerAddress (..), localServer, runWebSocket)
import Roon.Error (RoonError)
import Roon.Registry
import Roon.Transport (DisplayLines (..), NowPlaying (..), Zone (..), getZones)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

main :: IO ()
main =
  getArgs >>= \case
    [] -> probe localServer
    [host] -> probe localServer {host}
    [host, portText] | Just port <- readMaybe portText -> probe ServerAddress {host, port}
    _ -> do
      name <- getProgName
      hPutStrLn stderr ("usage: " <> name <> " [HOST [PORT]]   (default: 127.0.0.1 9330)")
      exitFailure

extension :: ExtensionInfo
extension =
  ExtensionInfo
    { extensionId = "io.github.guillaumedevolpiano.roon-api.probe"
    , displayName = "roon-api probe"
    , displayVersion = "0.1.0.0"
    , publisher = "roon-api"
    , email = "nobody@example.invalid"
    , website = Just "https://github.com/GuillaumedeVolpiano/roon-api"
    , requiredServices = ["com.roonlabs.transport:2", "com.roonlabs.browse:1"]
    , optionalServices = []
    , providedServices = [pingService]
    }

probe :: ServerAddress -> IO ()
probe address = do
  tokenFile <- defaultTokenFile extension.extensionId
  say ("Connecting to " <> Text.pack address.host <> ":" <> Text.pack (show address.port))
  outcome <-
    runEff . runConcurrent . runErrorNoCallStack @RoonError . runTokenStoreFile tokenFile $
      runWebSocket address . runMoo defaultRequestHandler $ do
        core <- info
        say ("Server: " <> core.displayName <> ", version " <> core.displayVersion)
        say ("Core id: " <> core.coreId)
        token <- loadToken core.coreId
        say $ case token of
          Just _ -> "Registering with the stored token"
          Nothing -> "Registering. Enable \"" <> extension.displayName <> "\" in Roon, under Settings, Extensions"
        registration <- register extension token
        saveToken registration.core.coreId registration.token
        say ("Registered. Token saved in " <> Text.pack tokenFile)
        say $ case registration.grantedServices of
          [] -> "Granted services: none"
          services -> "Granted services: " <> Text.intercalate ", " services
        zones <- getZones
        mapM_ (say . describe) zones
  case outcome of
    Right () -> pure ()
    Left err -> do
      hPutStrLn stderr ("roon-probe: " <> show err)
      exitFailure

describe :: Zone -> Text
describe zone =
  "Zone " <> zone.displayName <> " (" <> Text.pack (show zone.state) <> ")" <> case zone.nowPlaying of
    Nothing -> ""
    Just playing -> ": " <> Text.intercalate " / " (playing.threeLine.line1 : maybe [] pure playing.threeLine.line2 <> maybe [] pure playing.threeLine.line3)

say :: MonadIO m => Text -> m ()
say = liftIO . Text.putStrLn
