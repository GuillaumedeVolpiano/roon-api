-- | @roon-probe@: connects to a Roon Server, registers as an extension and
-- prints what the server answers, then the zones. A smoke test for the
-- library against a real server.
module Main (main) where

import Data.ByteString qualified as ByteString
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
import Effectful.FileSystem.IO.ByteString (hPutStrLn, writeFile)
import Roon.Effect.Moo (defaultRequestHandler, pingService, runMoo)
import Roon.Effect.TokenStore (defaultTokenFile, loadToken, runTokenStoreFile, saveToken)
import Roon.Effect.WebSocket (ServerAddress (..), localServer, runWebSocket, traceFrames)
import Roon.Error (RoonError)
import Roon.Browse (BrowseResult (..), Hierarchy (..), Item (..), ItemHint (..), List (..), LoadResult (..), Navigation (..), Session (..), browse, load)
import Roon.Connection (withRoon)
import Roon.Discovery (Core (..), defaultDiscovery, discover)
import Roon.Image (Image (..), defaultImageLimits, fetchImage, fitWithin, runImageHttp)
import Roon.Registry
import Roon.Transport (DisplayLines (..), NowPlaying (..), Zone (..), ZoneId, getZones)
import System.Exit (exitFailure)
import Prelude hiding (writeFile)
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
    ["discover"] -> do
      cores <- discover defaultDiscovery
      case cores of
        [] -> False <$ complain "No Roon Server answered."
        _ -> True <$ mapM_ (\core -> say (core.uniqueId <> "  " <> maybe "" id core.name <> "  " <> maybe "" id core.displayVersion <> "  " <> Text.pack core.address.host <> ":" <> Text.pack (show core.address.port))) cores
    "browse" : name : path | Just hierarchy <- lookup name hierarchies -> explore localServer hierarchy (map Text.pack path)
    ["image", key, file] -> saveImage localServer key file
    ["image", key, file, host] -> saveImage localServer {host} key file
    ["image", key, file, host, portText] | Just port <- readMaybe portText -> saveImage ServerAddress {host, port} key file
    [host] -> probe localServer {host}
    [host, portText] | Just port <- readMaybe portText -> probe ServerAddress {host, port}
    _ -> do
      name <- getProgName
      complain ("usage: " <> Text.pack name <> " [HOST [PORT] | discover | image KEY FILE [HOST [PORT]] | browse HIERARCHY [TITLE...]]   (default: 127.0.0.1 9330; ROON_TRACE=1 describes every frame; hierarchies: " <> Text.intercalate ", " (map (Text.pack . fst) hierarchies) <> ")")
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

hierarchies :: [(String, Hierarchy)]
hierarchies =
  [ ("browse", Browse)
  , ("playlists", Playlists)
  , ("internet_radio", InternetRadio)
  , ("albums", Albums)
  , ("artists", Artists)
  , ("genres", Genres)
  , ("composers", Composers)
  , ("search", Search)
  ]

-- | Walk a hierarchy of the local server by the titles of its items and
-- print the level reached. For @search@ the first title is the text to
-- search for. @<@ goes back one level and @#KEY@ opens an item by its key. Nothing is played: an item that is an action is not opened.
explore :: ServerAddress -> Hierarchy -> [Text] -> Eff Effects Bool
explore address hierarchy path = do
  tokenFile <- defaultTokenFile extension.extensionId
  outcome <- runErrorNoCallStack @RoonError . runTokenStoreFile tokenFile . withRoon address extension $ \_ -> do
    let current = Session {hierarchy, multiSessionKey = Just "roon-probe"}
        (input, titles) = case (hierarchy, path) of
          (Search, query : rest) -> (Just query, rest)
          _ -> (Nothing, path)
        walk [] = pure Nothing
        -- "<" goes back one level; "#KEY" opens an item by its key, even
        -- one of an earlier level.
        walk ("<" : rest) = browse current (Nothing :: Maybe ZoneId) (Back 1) >> walk rest
        walk (wanted : rest)
          | Just key <- Text.stripPrefix "#" wanted = browse current (Nothing :: Maybe ZoneId) (Select key) >>= \result -> case result of
              BrowseList _ -> walk rest
              other -> pure (Just ("opening the key " <> key <> " gave " <> Text.pack (show other)))
        walk (wanted : rest) = do
          level <- load current 0 500
          case [item | item <- level.items, Text.toCaseFold item.title == Text.toCaseFold wanted] of
            Item {hint = Just Action} : _ -> pure (Just ("\"" <> wanted <> "\" is an action: not opened"))
            Item {itemKey = Just key} : _ -> browse current (Nothing :: Maybe ZoneId) (Select key) >> walk rest
            _ -> pure (Just ("no item titled \"" <> wanted <> "\" at this level"))
    _ <- browse current (Nothing :: Maybe ZoneId) (Top input)
    stopped <- walk titles
    level <- load current 0 40
    say (level.list.title <> maybe "" (" / " <>) level.list.subtitle <> ": " <> Text.pack (show level.list.count) <> " items, level " <> Text.pack (show level.list.level) <> maybe "" ((", hint " <>) . Text.pack . show) level.list.hint)
    mapM_ (\item -> say ("  " <> maybe "-" (Text.pack . show) item.hint <> " | " <> item.title <> maybe "" (" | " <>) item.subtitle <> maybe "" (const " | image") item.imageKey <> maybe "" (const " | asks for input") item.inputPrompt <> maybe "" (" | #" <>) item.itemKey)) level.items
    pure stopped
  case outcome of
    Right Nothing -> pure True
    Right (Just problem) -> False <$ complain ("roon-probe: " <> problem)
    Left err -> False <$ complain ("roon-probe: " <> Text.pack (show err))

-- | Fetch the image of a key, at most 1000 pixels wide and high, and write
-- it to a file.
saveImage :: ServerAddress -> String -> FilePath -> Eff Effects Bool
saveImage address key file = do
  outcome <- runErrorNoCallStack @RoonError (runImageHttp defaultImageLimits (fetchImage address (Text.pack key) (fitWithin 1000)))
  case outcome of
    Left err -> False <$ complain ("roon-probe: " <> Text.pack (show err))
    Right image -> do
      writeFile file image.bytes
      say (Text.pack file <> ": " <> image.contentType <> ", " <> Text.pack (show (ByteString.length image.bytes)) <> " bytes")
      pure True

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
