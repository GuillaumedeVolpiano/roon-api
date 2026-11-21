-- | @roon-probe@: connects to a Roon Server, registers as an extension and
-- prints what the server answers, then the zones. A smoke test for the
-- library against a real server.
module Main (main) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Effectful
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Console.ByteString (Console, runConsole)
import Effectful.Console.ByteString qualified as Console
import Effectful.Environment (Environment, getArgs, getProgName, lookupEnv, runEnvironment)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.FileSystem.IO (stderr)
import Effectful.FileSystem.IO.ByteString (hPutStrLn)
import Roon.Effect.Moo (defaultRequestHandler, pingService, runMoo)
import Roon.Effect.TokenStore (defaultTokenFile, loadToken, runTokenStoreFile, saveToken)
import Roon.Effect.WebSocket (ServerAddress (..), localServer, runWebSocket, traceFrames)
import Roon.Error (RoonError)
import Roon.Registry
import Roon.Transport (DisplayLines (..), NowPlaying (..), Zone (..), getZones)
import System.Exit (exitFailure)
import Text.Read (readMaybe)

main :: IO ()
main = do
  succeeded <- runEff . runConcurrent . runFileSystem . runConsole . runEnvironment $ program
  if succeeded then pure () else exitFailure

type Effects = '[Environment, Console, FileSystem, Concurrent, IOE]

-- | 'False' when the probe failed.
program :: Eff Effects Bool
program =
  getArgs >>= \case
    [] -> probe localServer
    [host] -> probe localServer {host}
    [host, portText] | Just port <- readMaybe portText -> probe ServerAddress {host, port}
    _ -> do
      name <- getProgName
      complain ("usage: " <> Text.pack name <> " [HOST [PORT]]   (default: 127.0.0.1 9330; ROON_TRACE=1 describes every frame)")
      pure False

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

probe :: ServerAddress -> Eff Effects Bool
probe address = do
  tokenFile <- defaultTokenFile extension.extensionId
  -- ROON_TRACE, when set, describes every frame on stderr.
  tracing <- maybe False (not . null) <$> lookupEnv "ROON_TRACE"
  let trace = if tracing then traceFrames 300 complain else id
  say ("Connecting to " <> Text.pack address.host <> ":" <> Text.pack (show address.port))
  outcome <-
    runErrorNoCallStack @RoonError . runTokenStoreFile tokenFile $
      runWebSocket address . trace . runMoo defaultRequestHandler $ do
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
    Right () -> pure True
    Left err -> False <$ complain ("roon-probe: " <> Text.pack (show err))

describe :: Zone -> Text
describe zone =
  "Zone " <> zone.displayName <> " (" <> Text.pack (show zone.state) <> ")" <> case zone.nowPlaying of
    Nothing -> ""
    Just playing -> ": " <> Text.intercalate " / " (playing.threeLine.line1 : maybe [] pure playing.threeLine.line2 <> maybe [] pure playing.threeLine.line3)

say :: Console :> es => Text -> Eff es ()
say = Console.putStrLn . encodeUtf8

complain :: FileSystem :> es => Text -> Eff es ()
complain = hPutStrLn stderr . encodeUtf8
