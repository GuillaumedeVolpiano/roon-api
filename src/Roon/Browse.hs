-- |
-- Module      : Roon.Browse
-- Description : Browsing the library, searching, and playing what is found
-- License     : BSD-3-Clause
--
-- The browse service, @com.roonlabs.browse:1@, presents the library as
-- lists: the server keeps a stack of levels for each browsing session,
-- 'browse' moves in that stack, and 'load' reads the items of a level.
--
-- Item keys are only valid while the session stays where it was when they
-- were loaded. A session is identified by its hierarchy and, to run
-- several at once, a key of its own: see t'Session'.
--
-- Categories and actions are only known by their titles, such as
-- @Tracks@ or @Play Now@. Those of this module are the English ones; they
-- may follow the language of the Roon interface.
module Roon.Browse
  ( browseService

    -- * Sessions
  , Hierarchy (..)
  , Session (..)
  , session

    -- * Lists and items
  , List (..)
  , ListHint (..)
  , Item (..)
  , ItemHint (..)
  , InputPrompt (..)

    -- * Browsing
  , Navigation (..)
  , BrowseResult (..)
  , browse
  , LoadResult (..)
  , load

    -- * Searching for tracks
  , Candidate (..)
  , SearchOptions (..)
  , defaultSearchOptions
  , searchTracks
  , searchTracksConcurrently

    -- * Acting on a track
  , TrackAction (..)
  , trackActionTitle
  , performTrackAction
  , performAction
  , performTrackActions
  ) where

import Data.Aeson
  ( FromJSON (..)
  , object
  , withObject
  , withText
  , (.:)
  , (.:?)
  , (.!=)
  , (.=)
  )
import Data.Aeson.Types (Pair)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as Text
import Effectful
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.Streamly.Stream qualified as Concurrent
import Roon.Effect.Moo (Moo, request)
import Roon.Error (RoonError (..))
import Roon.Moo.Message (Body (..))
import Roon.Service (expect)
import Roon.Transport (ZoneOrOutput (..))
import Streamly.Data.Stream qualified as Stream

-- | The name of the browse service, version included.
browseService :: Text
browseService = "com.roonlabs.browse:1"

-- Sessions

-- | The hierarchies the reference implementation lists.
data Hierarchy
  = Browse
  | Playlists
  | Settings
  | InternetRadio
  | Albums
  | Artists
  | Genres
  | Composers
  | Search
  deriving stock (Eq, Show, Enum, Bounded)

hierarchyName :: Hierarchy -> Text
hierarchyName = \case
  Browse -> "browse"
  Playlists -> "playlists"
  Settings -> "settings"
  InternetRadio -> "internet_radio"
  Albums -> "albums"
  Artists -> "artists"
  Genres -> "genres"
  Composers -> "composers"
  Search -> "search"

-- | A browsing session: a hierarchy, and a key to tell apart several
-- sessions of the same hierarchy.
data Session = Session
  { hierarchy :: Hierarchy
  , multiSessionKey :: Maybe Text
  }
  deriving stock (Eq, Show)

-- | The default session of a hierarchy.
session :: Hierarchy -> Session
session hierarchy = Session {hierarchy, multiSessionKey = Nothing}

sessionFields :: Session -> [Pair]
sessionFields Session {hierarchy, multiSessionKey} =
  ("hierarchy" .= hierarchyName hierarchy) : catMaybes [("multi_session_key" .=) <$> multiSessionKey]

-- Lists and items

-- | A level of the browsing stack.
data List = List
  { title :: Text
  , subtitle :: Maybe Text
  , count :: Int
  -- ^ How many items the level holds.
  , level :: Int
  -- ^ 0 at the top.
  , imageKey :: Maybe Text
  , displayOffset :: Maybe Int
  , hint :: Maybe ListHint
  }
  deriving stock (Eq, Show)

instance FromJSON List where
  parseJSON = withObject "List" $ \fields ->
    List
      <$> fields .: "title"
      <*> fields .:? "subtitle"
      <*> fields .: "count"
      <*> fields .: "level"
      <*> fields .:? "image_key"
      <*> fields .:? "display_offset"
      <*> fields .:? "hint"

-- | Unknown hints are kept as they are named; the reference asks to treat
-- them as no hint.
data ListHint = ActionListHint | OtherListHint Text
  deriving stock (Eq, Show)

instance FromJSON ListHint where
  parseJSON = withText "ListHint" $ \case
    "action_list" -> pure ActionListHint
    other -> pure (OtherListHint other)

-- | An item of a list.
data Item = Item
  { title :: Text
  , subtitle :: Maybe Text
  -- ^ For a track, its artists.
  , imageKey :: Maybe Text
  , itemKey :: Maybe Text
  -- ^ What to select to open the item.
  , hint :: Maybe ItemHint
  , inputPrompt :: Maybe InputPrompt
  -- ^ Present when opening the item needs some input.
  }
  deriving stock (Eq, Show)

instance FromJSON Item where
  parseJSON = withObject "Item" $ \fields ->
    Item
      <$> fields .: "title"
      <*> fields .:? "subtitle"
      <*> fields .:? "image_key"
      <*> fields .:? "item_key"
      <*> fields .:? "hint"
      <*> fields .:? "input_prompt"

-- | What selecting an item does.
data ItemHint
  = -- | Selecting the item does something, such as playing.
    Action
  | -- | Opens a list of actions.
    ActionList
  | -- | Opens a list.
    ListItem
  | -- | Only displayed.
    Header
  | OtherItemHint Text
  deriving stock (Eq, Show)

instance FromJSON ItemHint where
  parseJSON = withText "ItemHint" $ \case
    "action" -> pure Action
    "action_list" -> pure ActionList
    "list" -> pure ListItem
    "header" -> pure Header
    other -> pure (OtherItemHint other)

-- | The input an item asks for before it opens, such as the text of a
-- search.
data InputPrompt = InputPrompt
  { prompt :: Text
  , action :: Text
  , value :: Maybe Text
  , isPassword :: Bool
  }
  deriving stock (Eq, Show)

instance FromJSON InputPrompt where
  parseJSON = withObject "InputPrompt" $ \fields ->
    InputPrompt
      <$> fields .: "prompt"
      <*> fields .: "action"
      <*> fields .:? "value"
      <*> fields .:? "is_password" .!= False

-- Browsing

-- | Where to go in a session.
data Navigation
  = -- | Open an item.
    Select Text
  | -- | Open an item that asks for input, with that input.
    SelectWithInput Text Text
  | -- | Go back to the top of the hierarchy. For 'Search', the input is
    -- the text to search for.
    Top (Maybe Text)
  | -- | Go back some levels.
    Back Int
  | -- | Reload the current level.
    Reload
  deriving stock (Eq, Show)

navigationFields :: Navigation -> [Pair]
navigationFields = \case
  Select key -> ["item_key" .= key]
  SelectWithInput key input -> ["item_key" .= key, "input" .= input]
  Top input -> ("pop_all" .= True) : catMaybes [("input" .=) <$> input]
  Back levels -> ["pop_levels" .= levels]
  Reload -> ["refresh_list" .= True]

-- | What to show after a move.
data BrowseResult
  = -- | A message for the user; 'True' for an error.
    BrowseMessage Text Bool
  | BrowseNone
  | -- | The current level changed: 'load' its items.
    BrowseList List
  | -- | Replace the selected item.
    BrowseReplaceItem Item
  | -- | Remove the selected item.
    BrowseRemoveItem
  | -- | An action not in the reference, kept as named.
    BrowseOther Text
  deriving stock (Eq, Show)

instance FromJSON BrowseResult where
  parseJSON = withObject "BrowseResult" $ \fields ->
    fields .: "action" >>= \case
      ("message" :: Text) -> BrowseMessage <$> fields .:? "message" .!= "" <*> fields .:? "is_error" .!= False
      "none" -> pure BrowseNone
      "list" -> BrowseList <$> fields .: "list"
      "replace_item" -> BrowseReplaceItem <$> fields .: "item"
      "remove_item" -> pure BrowseRemoveItem
      other -> pure (BrowseOther other)

-- | Move in a session. The zone matters for actions that play.
browse
  :: (ZoneOrOutput target, Moo :> es, Error RoonError :> es)
  => Session
  -> Maybe target
  -> Navigation
  -> Eff es BrowseResult
browse current target navigation =
  request name (JsonBody body) >>= expect name "Success"
  where
    name = browseService <> "/browse"
    body =
      object $
        sessionFields current
          <> navigationFields navigation
          <> catMaybes [("zone_or_output_id" .=) . zoneOrOutputId <$> target]

-- | A part of a list: the list, where the part starts, and its items.
data LoadResult = LoadResult
  { list :: List
  , offset :: Int
  , items :: [Item]
  }
  deriving stock (Eq, Show)

instance FromJSON LoadResult where
  parseJSON = withObject "LoadResult" $ \fields ->
    LoadResult <$> fields .: "list" <*> fields .:? "offset" .!= 0 <*> fields .:? "items" .!= []

-- | Read items of the current level, from an offset.
load :: (Moo :> es, Error RoonError :> es) => Session -> Int -> Int -> Eff es LoadResult
load current offset count =
  request name (JsonBody (object (sessionFields current <> ["offset" .= offset, "count" .= count]))) >>= expect name "Success"
  where
    name = browseService <> "/load"

-- Searching for tracks

-- | A track found by a search, and the session where its key is valid.
data Candidate = Candidate
  { session :: Session
  , item :: Item
  }
  deriving stock (Eq, Show)

-- | How to read the results of a search for tracks.
data SearchOptions = SearchOptions
  { tracksTitle :: Text
  -- ^ The title of the category of tracks in the results.
  , maxCandidates :: Int
  }
  deriving stock (Eq, Show)

-- | The English category, and ten candidates.
defaultSearchOptions :: SearchOptions
defaultSearchOptions = SearchOptions {tracksTitle = "Tracks", maxCandidates = 10}

-- | Search for tracks in a session, which this moves. Returns the tracks
-- of the category of tracks, best first; none if the search found no
-- track.
searchTracks
  :: (Moo :> es, Error RoonError :> es)
  => SearchOptions
  -> Session
  -> Text
  -> Eff es [Candidate]
searchTracks options current query = do
  _ <- browse current noTarget (Top (Just query))
  top <- load current 0 100
  case [key | Item {title, hint = Just ListItem, itemKey = Just key} <- top.items, title == options.tracksTitle] of
    [] -> pure []
    key : _ -> do
      _ <- browse current noTarget (Select key)
      tracks <- load current 0 options.maxCandidates
      pure [Candidate {session = current, item} | item@Item {hint = Just ActionList} <- tracks.items]

-- | Run several searches at once, each in a session of its own, keyed by
-- the prefix and the position of the query. A failed search does not stop
-- the others: each query gets its own outcome.
searchTracksConcurrently
  :: (IOE :> es, Moo :> es)
  => SearchOptions
  -> Text
  -- ^ The prefix of the session keys.
  -> [Text]
  -> Eff es [(Text, Either RoonError [Candidate])]
searchTracksConcurrently options prefix queries =
  Stream.toList $
    Concurrent.parMapM (Concurrent.ordered True) search (Stream.fromList (zip [0 :: Int ..] queries))
  where
    search (index, query) =
      (query,) <$> runErrorNoCallStack (searchTracks options (Session Search (Just (prefix <> Text.pack (show index)))) query)

-- Acting on a track

-- | What can be done with a track.
data TrackAction = PlayNow | AddNext | Queue | StartRadio
  deriving stock (Eq, Show, Enum, Bounded)

-- | The English title of an action.
trackActionTitle :: TrackAction -> Text
trackActionTitle = \case
  PlayNow -> "Play Now"
  AddNext -> "Add Next"
  Queue -> "Queue"
  StartRadio -> "Start Radio"

-- | Open a candidate down to its list of actions, then select the action
-- with the given title, for a zone. The session must not have moved since
-- the search. After an action, Roon goes back to the level of the track;
-- the zone reports the change a moment later. Throws 'BrowseFailed' when Roon reports an error, and
-- 'BrowseItemMissing' when no such action is offered.
performTrackAction
  :: (ZoneOrOutput target, Moo :> es, Error RoonError :> es)
  => target
  -> Text
  -- ^ The title of the action, such as @trackActionTitle Queue@.
  -> Candidate
  -> Eff es ()
performTrackAction = performAction

-- | Like 'performTrackAction', for anything Roon can play: a track, but
-- also an album, a playlist, an artist or a radio.
--
-- Observed with Roon Server 2.75: an album and a playlist open on a level
-- whose first item, @Play Album@ or @Play Playlist@, leads to the actions,
-- before the tracks; a playlist also offers @Shuffle@. An album found by a
-- search first opens on a level that holds only itself. A key of an earlier
-- level is still accepted after the session has gone deeper: Roon goes
-- back by itself. A key that Roon no longer knows is answered
-- @InvalidItemKey@ ('UnexpectedReply').
performAction
  :: (ZoneOrOutput target, Moo :> es, Error RoonError :> es)
  => target
  -> Text
  -- ^ The title of the action, such as @trackActionTitle Queue@.
  -> Candidate
  -> Eff es ()
performAction target actionTitle candidate = case candidate.item.itemKey of
  Nothing -> throwError (BrowseItemMissing ("no key for " <> candidate.item.title))
  Just key -> descend (4 :: Int) key
  where
    current = candidate.session
    missing = throwError (BrowseItemMissing (actionTitle <> " for " <> candidate.item.title))
    -- A track opens on a level that holds only itself, then on its
    -- actions; an album or a playlist on a level that starts with the item
    -- that plays the whole of it. The depth bounds the descent.
    descend 0 _ = throwError (BrowseItemMissing ("no action list for " <> candidate.item.title))
    descend depth key = do
      _ <- browse current (Just target) (Select key)
      level <- load current 0 100
      case [actionKey | Item {title, hint = Just Action, itemKey = Just actionKey} <- level.items, title == actionTitle] of
        actionKey : _ ->
          browse current (Just target) (Select actionKey) >>= \case
            BrowseMessage message True -> throwError (BrowseFailed message)
            _ -> pure ()
        []
          | level.list.hint == Just ActionListHint -> missing
          | otherwise -> case [(title, next) | Item {title, hint = Just ActionList, itemKey = Just next} <- level.items] of
              [(_, next)] -> descend (depth - 1) next
              (title, next) : _ | "Play " `Text.isPrefixOf` title -> descend (depth - 1) next
              -- An album found by a search opens on a level that holds
              -- only itself, as a list, before its tracks.
              [] | [Item {hint = Just ListItem, itemKey = Just next}] <- level.items -> descend (depth - 1) next
              _ -> missing

-- | Perform actions one after the other, in order, and report each one.
-- A failure does not stop the next ones.
performTrackActions
  :: (ZoneOrOutput target, Moo :> es)
  => target
  -> [(Text, Candidate)]
  -> Eff es [(Candidate, Either RoonError ())]
performTrackActions target =
  traverse (\(actionTitle, candidate) -> (candidate,) <$> runErrorNoCallStack (performTrackAction target actionTitle candidate))

noTarget :: Maybe NoTarget
noTarget = Nothing

-- | No zone, for moves that do not play.
data NoTarget

instance ZoneOrOutput NoTarget where
  zoneOrOutputId = \case {}
