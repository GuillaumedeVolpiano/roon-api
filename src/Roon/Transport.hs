{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Roon.Transport
-- Description : Zones, outputs and playback control
-- License     : BSD-3-Clause
--
-- The transport service, @com.roonlabs.transport:2@: what each zone plays,
-- its queue, and the controls on playback, volume and zone settings.
--
-- A zone plays one queue through one or more outputs, grouped. Requests
-- that act on a zone also accept one of its outputs ('ZoneOrOutput').
module Roon.Transport
  ( transportService

    -- * Identifiers
  , ZoneId (..)
  , OutputId (..)
  , ZoneOrOutput (..)

    -- * Zones and outputs
  , Zone (..)
  , PlaybackState (..)
  , ZoneSettings (..)
  , Loop (..)
  , NowPlaying (..)
  , DisplayLines (..)
  , Output (..)
  , Volume (..)
  , VolumeType (..)
  , SourceControl (..)
  , SourceControlStatus (..)
  , getZones
  , getOutputs

    -- * Following the zones
  , ZoneEvent (..)
  , ZoneChanges (..)
  , SeekChange (..)
  , subscribeZones
  , applyZoneEvent
  , zones

    -- * The queue
  , QueueItem (..)
  , QueueEvent (..)
  , QueueChange (..)
  , subscribeQueue
  , applyQueueEvent
  , queue
  , playFromHere

    -- * Controls
  , Control (..)
  , control
  , pauseAll
  , Seek (..)
  , seek
  , SettingsChange (..)
  , LoopChange (..)
  , noSettingsChange
  , changeSettings
  , VolumeChange (..)
  , changeVolume
  , mute
  , muteAll
  , transferZone
  ) where

import Data.Aeson
  ( FromJSON (..)
  , ToJSON (..)
  , Value
  , object
  , withObject
  , withText
  , (.:)
  , (.:?)
  , (.!=)
  , (.=)
  )
import Data.Aeson.Types (Pair)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Effectful
import Effectful.Error.Static (Error)
import Roon.Effect.Moo (Moo, request)
import Roon.Error (RoonError)
import Roon.Moo.Message (Body (..))
import Roon.Service (call, expect, subscribe, subscriptionUpdates)
import Streamly.Data.Fold (Fold)
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream (Stream)

transportService :: Text
transportService = "com.roonlabs.transport:2"

-- Identifiers

newtype ZoneId = ZoneId Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, FromJSON, ToJSON)

newtype OutputId = OutputId Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, FromJSON, ToJSON)

-- | What requests that act on a zone accept: the zone, or one of its
-- outputs.
class ZoneOrOutput a where
  zoneOrOutputId :: a -> Text

instance ZoneOrOutput ZoneId where
  zoneOrOutputId (ZoneId zoneId) = zoneId

instance ZoneOrOutput OutputId where
  zoneOrOutputId (OutputId outputId) = outputId

-- Zones and outputs

data Zone = Zone
  { zoneId :: ZoneId
  , displayName :: Text
  , outputs :: [Output]
  , state :: PlaybackState
  , isPreviousAllowed :: Bool
  , isNextAllowed :: Bool
  , isPauseAllowed :: Bool
  , isPlayAllowed :: Bool
  , isSeekAllowed :: Bool
  , queueItemsRemaining :: Maybe Int
  , queueTimeRemaining :: Maybe Int
  -- ^ In seconds.
  , settings :: Maybe ZoneSettings
  , nowPlaying :: Maybe NowPlaying
  -- ^ Absent when nothing is loaded.
  }
  deriving stock (Eq, Show)

instance FromJSON Zone where
  parseJSON = withObject "Zone" $ \fields ->
    Zone
      <$> fields .: "zone_id"
      <*> fields .: "display_name"
      <*> fields .:? "outputs" .!= []
      <*> fields .: "state"
      <*> fields .:? "is_previous_allowed" .!= False
      <*> fields .:? "is_next_allowed" .!= False
      <*> fields .:? "is_pause_allowed" .!= False
      <*> fields .:? "is_play_allowed" .!= False
      <*> fields .:? "is_seek_allowed" .!= False
      <*> fields .:? "queue_items_remaining"
      <*> fields .:? "queue_time_remaining"
      <*> fields .:? "settings"
      <*> fields .:? "now_playing"

-- | Unknown states are kept as they are named.
data PlaybackState = Playing | Paused | Loading | Stopped | OtherState Text
  deriving stock (Eq, Show)

instance FromJSON PlaybackState where
  parseJSON = withText "PlaybackState" $ \case
    "playing" -> pure Playing
    "paused" -> pure Paused
    "loading" -> pure Loading
    "stopped" -> pure Stopped
    other -> pure (OtherState other)

data ZoneSettings = ZoneSettings
  { loop :: Loop
  , shuffle :: Bool
  , autoRadio :: Bool
  }
  deriving stock (Eq, Show)

instance FromJSON ZoneSettings where
  parseJSON = withObject "ZoneSettings" $ \fields ->
    ZoneSettings <$> fields .: "loop" <*> fields .: "shuffle" <*> fields .: "auto_radio"

data Loop = LoopQueue | LoopOne | LoopDisabled | OtherLoop Text
  deriving stock (Eq, Show)

instance FromJSON Loop where
  parseJSON = withText "Loop" $ \case
    "loop" -> pure LoopQueue
    "loop_one" -> pure LoopOne
    "disabled" -> pure LoopDisabled
    other -> pure (OtherLoop other)

data NowPlaying = NowPlaying
  { seekPosition :: Maybe Int
  -- ^ In seconds.
  , length :: Maybe Int
  -- ^ In seconds.
  , imageKey :: Maybe Text
  , artistImageKeys :: [Text]
  , oneLine :: DisplayLines
  , twoLine :: DisplayLines
  , threeLine :: DisplayLines
  -- ^ For three lines, Roon gives the title, the artists and the album.
  }
  deriving stock (Eq, Show)

instance FromJSON NowPlaying where
  parseJSON = withObject "NowPlaying" $ \fields ->
    NowPlaying
      <$> fields .:? "seek_position"
      <*> fields .:? "length"
      <*> fields .:? "image_key"
      <*> fields .:? "artist_image_keys" .!= []
      <*> fields .: "one_line"
      <*> fields .: "two_line"
      <*> fields .: "three_line"

-- | The text Roon prepares for displays of one, two or three lines.
data DisplayLines = DisplayLines
  { line1 :: Text
  , line2 :: Maybe Text
  , line3 :: Maybe Text
  }
  deriving stock (Eq, Show)

instance FromJSON DisplayLines where
  parseJSON = withObject "DisplayLines" $ \fields ->
    DisplayLines <$> fields .: "line1" <*> fields .:? "line2" <*> fields .:? "line3"

data Output = Output
  { outputId :: OutputId
  , zoneId :: ZoneId
  , displayName :: Text
  , volume :: Maybe Volume
  -- ^ Absent when Roon does not control the volume of this output.
  , sourceControls :: [SourceControl]
  , canGroupWithOutputIds :: [OutputId]
  }
  deriving stock (Eq, Show)

instance FromJSON Output where
  parseJSON = withObject "Output" $ \fields ->
    Output
      <$> fields .: "output_id"
      <*> fields .: "zone_id"
      <*> fields .: "display_name"
      <*> fields .:? "volume"
      <*> fields .:? "source_controls" .!= []
      <*> fields .:? "can_group_with_output_ids" .!= []

-- | Volume values are not integers, and their range may extend on both
-- sides of zero. An incremental control only has the type: it is driven
-- by steps of one, up or down ('RelativeStep').
data Volume = Volume
  { volumeType :: VolumeType
  , minimum :: Maybe Double
  , maximum :: Maybe Double
  , value :: Maybe Double
  , step :: Maybe Double
  , isMuted :: Maybe Bool
  }
  deriving stock (Eq, Show)

instance FromJSON Volume where
  parseJSON = withObject "Volume" $ \fields ->
    Volume
      <$> fields .:? "type" .!= VolumeNumber
      <*> fields .:? "min"
      <*> fields .:? "max"
      <*> fields .:? "value"
      <*> fields .:? "step"
      <*> fields .:? "is_muted"

-- | The reference documentation asks to treat an unknown type like a
-- number; it is kept here as named.
data VolumeType = VolumeDecibel | VolumeNumber | VolumeIncremental | OtherVolumeType Text
  deriving stock (Eq, Show)

instance FromJSON VolumeType where
  parseJSON = withText "VolumeType" $ \case
    "db" -> pure VolumeDecibel
    "number" -> pure VolumeNumber
    "incremental" -> pure VolumeIncremental
    other -> pure (OtherVolumeType other)

-- | A device that Roon can switch on, off or to its input, such as an
-- amplifier.
data SourceControl = SourceControl
  { controlKey :: Text
  , displayName :: Text
  , status :: SourceControlStatus
  , supportsStandby :: Bool
  }
  deriving stock (Eq, Show)

instance FromJSON SourceControl where
  parseJSON = withObject "SourceControl" $ \fields ->
    SourceControl
      <$> fields .: "control_key"
      <*> fields .: "display_name"
      <*> fields .: "status"
      <*> fields .:? "supports_standby" .!= False

data SourceControlStatus = Selected | Deselected | Standby | Indeterminate | OtherStatus Text
  deriving stock (Eq, Show)

instance FromJSON SourceControlStatus where
  parseJSON = withText "SourceControlStatus" $ \case
    "selected" -> pure Selected
    "deselected" -> pure Deselected
    "standby" -> pure Standby
    "indeterminate" -> pure Indeterminate
    other -> pure (OtherStatus other)

newtype ZoneList = ZoneList [Zone]

instance FromJSON ZoneList where
  parseJSON = withObject "zones" $ \fields -> ZoneList <$> fields .:? "zones" .!= []

newtype OutputList = OutputList [Output]

instance FromJSON OutputList where
  parseJSON = withObject "outputs" $ \fields -> OutputList <$> fields .:? "outputs" .!= []

-- | The zones, as they are now.
getZones :: (Moo :> es, Error RoonError :> es) => Eff es [Zone]
getZones = (\(ZoneList list) -> list) <$> (request name NoBody >>= expect name "Success")
  where
    name = method "get_zones"

-- | The outputs, as they are now.
getOutputs :: (Moo :> es, Error RoonError :> es) => Eff es [Output]
getOutputs = (\(OutputList list) -> list) <$> (request name NoBody >>= expect name "Success")
  where
    name = method "get_outputs"

-- Following the zones

-- | What a zone subscription reports: every zone first, then changes.
data ZoneEvent
  = ZonesSubscribed [Zone]
  | ZonesChanged ZoneChanges
  deriving stock (Eq, Show)

data ZoneChanges = ZoneChanges
  { removed :: [ZoneId]
  , added :: [Zone]
  , changed :: [Zone]
  -- ^ Complete zones, which replace the previous ones.
  , seekChanged :: [SeekChange]
  -- ^ Sent every second while a zone plays.
  }
  deriving stock (Eq, Show)

instance FromJSON ZoneChanges where
  parseJSON = withObject "ZoneChanges" $ \fields ->
    ZoneChanges
      <$> fields .:? "zones_removed" .!= []
      <*> fields .:? "zones_added" .!= []
      <*> fields .:? "zones_changed" .!= []
      <*> fields .:? "zones_seek_changed" .!= []

data SeekChange = SeekChange
  { zoneId :: ZoneId
  , seekPosition :: Maybe Int
  , queueTimeRemaining :: Maybe Int
  }
  deriving stock (Eq, Show)

instance FromJSON SeekChange where
  parseJSON = withObject "SeekChange" $ \fields ->
    SeekChange <$> fields .: "zone_id" <*> fields .:? "seek_position" <*> fields .:? "queue_time_remaining"

-- | Follow the zones. The stream ends if the server ends the
-- subscription; returning from the consumer ends it.
subscribeZones
  :: (Moo :> es, Error RoonError :> es)
  => (Stream (Eff es) ZoneEvent -> Eff es a)
  -> Eff es a
subscribeZones consume =
  subscribe transportService "zones" [] $
    consume . subscriptionUpdates (method "subscribe_zones") (\(ZoneList list) -> ZonesSubscribed list) ZonesChanged

-- | Bring a view of the zones up to date.
applyZoneEvent :: ZoneEvent -> Map ZoneId Zone -> Map ZoneId Zone
applyZoneEvent = \case
  ZonesSubscribed list -> const (byId list)
  ZonesChanged changes ->
    flip (foldr applySeek) changes.seekChanged
      . Map.union (byId (changes.changed <> changes.added))
      . flip (foldr Map.delete) changes.removed
  where
    byId list = Map.fromList [(zone.zoneId, zone) | zone <- list]
    applySeek change = Map.adjust (seekTo change) change.zoneId
    -- Built with the constructors: the field names are shared with
    -- 'SeekChange', which makes record updates ambiguous.
    seekTo change Zone {..} =
      Zone {nowPlaying = atSeek change.seekPosition <$> nowPlaying, queueTimeRemaining = change.queueTimeRemaining, ..}
    atSeek position (NowPlaying _ len image artists one two three) = NowPlaying position len image artists one two three

-- | The zones as the events describe them, for 'Streamly.Data.Stream.scan'
-- or 'Streamly.Data.Stream.fold'.
zones :: Monad m => Fold m ZoneEvent (Map ZoneId Zone)
zones = Fold.foldl' (flip applyZoneEvent) Map.empty

-- The queue

data QueueItem = QueueItem
  { queueItemId :: Int
  , length :: Maybe Int
  -- ^ In seconds.
  , imageKey :: Maybe Text
  , oneLine :: DisplayLines
  , twoLine :: DisplayLines
  , threeLine :: DisplayLines
  }
  deriving stock (Eq, Show)

instance FromJSON QueueItem where
  parseJSON = withObject "QueueItem" $ \fields ->
    QueueItem
      <$> fields .: "queue_item_id"
      <*> fields .:? "length"
      <*> fields .:? "image_key"
      <*> fields .: "one_line"
      <*> fields .: "two_line"
      <*> fields .: "three_line"

-- | What a queue subscription reports: the first items, then changes.
data QueueEvent
  = QueueSubscribed [QueueItem]
  | QueueChanged [QueueChange]
  deriving stock (Eq, Show)

-- | One change to the items of a queue, at a position counted from zero.
-- When a track ends, Roon removes the items it reported and inserts the
-- new ones.
data QueueChange
  = QueueRemove {index :: Int, count :: Int}
  | QueueInsert {index :: Int, items :: [QueueItem]}
  | -- | An operation not seen so far, kept as it came.
    OtherQueueChange Value
  deriving stock (Eq, Show)

instance FromJSON QueueChange where
  parseJSON value = flip (withObject "QueueChange") value $ \fields ->
    fields .: "operation" >>= \case
      ("remove" :: Text) -> QueueRemove <$> fields .: "index" <*> fields .: "count"
      "insert" -> QueueInsert <$> fields .: "index" <*> fields .: "items"
      _ -> pure (OtherQueueChange value)

newtype QueueChanges = QueueChanges [QueueChange]

instance FromJSON QueueChanges where
  parseJSON = withObject "QueueChanges" $ \fields -> QueueChanges <$> fields .:? "changes" .!= []

newtype QueueItems = QueueItems [QueueItem]

instance FromJSON QueueItems where
  parseJSON = withObject "queue" $ \fields -> QueueItems <$> fields .:? "items" .!= []

-- | Follow the queue of a zone, up to a number of items.
subscribeQueue
  :: (ZoneOrOutput target, Moo :> es, Error RoonError :> es)
  => target
  -> Int
  -- ^ The most items to report.
  -> (Stream (Eff es) QueueEvent -> Eff es a)
  -> Eff es a
subscribeQueue target maxItems consume =
  subscribe transportService "queue" ["zone_or_output_id" .= zoneOrOutputId target, "max_item_count" .= maxItems] $
    consume . subscriptionUpdates (method "subscribe_queue") (\(QueueItems items) -> QueueSubscribed items) (\(QueueChanges changes) -> QueueChanged changes)

-- | Bring a view of the queue up to date. An 'OtherQueueChange' leaves the
-- items as they are, and the view may then be wrong.
applyQueueEvent :: QueueEvent -> [QueueItem] -> [QueueItem]
applyQueueEvent = \case
  QueueSubscribed items -> const items
  QueueChanged changes -> \items -> foldl (flip applyChange) items changes
  where
    applyChange = \case
      QueueRemove {index, count} -> \items -> take index items <> drop (index + count) items
      QueueInsert {index, items = inserted} -> \items -> take index items <> inserted <> drop index items
      OtherQueueChange _ -> id

-- | The items as the events describe them.
queue :: Monad m => Fold m QueueEvent [QueueItem]
queue = Fold.foldl' (flip applyQueueEvent) []

-- | Play the queue from one of its items.
playFromHere :: (ZoneOrOutput target, Moo :> es, Error RoonError :> es) => target -> Int -> Eff es ()
playFromHere target queueItemId =
  call (method "play_from_here") $
    object ["zone_or_output_id" .= zoneOrOutputId target, "queue_item_id" .= queueItemId]

-- Controls

data Control
  = -- | Start playback if paused or stopped.
    Play
  | -- | Pause playback if playing or loading.
    Pause
  | -- | Play if paused or stopped, pause otherwise.
    PlayPause
  | -- | Stop playback and release the audio device.
    Stop
  | -- | Go to the start of the track, or to the previous one.
    Previous
  | Next
  deriving stock (Eq, Show, Enum, Bounded)

controlName :: Control -> Text
controlName = \case
  Play -> "play"
  Pause -> "pause"
  PlayPause -> "playpause"
  Stop -> "stop"
  Previous -> "previous"
  Next -> "next"

-- | Operate a zone. Check the matching @is…Allowed@ field of the zone
-- first.
control :: (ZoneOrOutput target, Moo :> es, Error RoonError :> es) => target -> Control -> Eff es ()
control target which =
  call (method "control") $
    object ["zone_or_output_id" .= zoneOrOutputId target, "control" .= controlName which]

pauseAll :: (Moo :> es, Error RoonError :> es) => Eff es ()
pauseAll = call (method "pause_all") (object [])

-- | A seek position, in seconds.
data Seek = SeekTo Int | SeekBy Int
  deriving stock (Eq, Show)

seek :: (ZoneOrOutput target, Moo :> es, Error RoonError :> es) => target -> Seek -> Eff es ()
seek target position =
  call (method "seek") . object $
    ("zone_or_output_id" .= zoneOrOutputId target) : case position of
      SeekTo seconds -> ["how" .= ("absolute" :: Text), "seconds" .= seconds]
      SeekBy seconds -> ["how" .= ("relative" :: Text), "seconds" .= seconds]

-- | The settings to change; 'Nothing' leaves one as it is.
data SettingsChange = SettingsChange
  { setShuffle :: Maybe Bool
  , setAutoRadio :: Maybe Bool
  , setLoop :: Maybe LoopChange
  }
  deriving stock (Eq, Show)

data LoopChange = SetLoop Loop | NextLoop
  deriving stock (Eq, Show)

noSettingsChange :: SettingsChange
noSettingsChange = SettingsChange {setShuffle = Nothing, setAutoRadio = Nothing, setLoop = Nothing}

changeSettings :: (ZoneOrOutput target, Moo :> es, Error RoonError :> es) => target -> SettingsChange -> Eff es ()
changeSettings target change =
  call (method "change_settings") . object $
    ("zone_or_output_id" .= zoneOrOutputId target)
      : catMaybes
        [ ("shuffle" .=) <$> change.setShuffle
        , ("auto_radio" .=) <$> change.setAutoRadio
        , ("loop" .=) . loopName <$> change.setLoop
        ]
  where
    loopName :: LoopChange -> Text
    loopName = \case
      NextLoop -> "next"
      SetLoop LoopQueue -> "loop"
      SetLoop LoopOne -> "loop_one"
      SetLoop LoopDisabled -> "disabled"
      SetLoop (OtherLoop name) -> name

data VolumeChange
  = -- | Set the volume, in the units of the output.
    VolumeTo Double
  | -- | Move the volume, in the units of the output.
    VolumeBy Double
  | -- | Move the volume by steps of the output.
    RelativeStep Int
  deriving stock (Eq, Show)

changeVolume :: (Moo :> es, Error RoonError :> es) => OutputId -> VolumeChange -> Eff es ()
changeVolume outputId change =
  call (method "change_volume") . object $
    ("output_id" .= outputId) : case change of
      VolumeTo level -> how "absolute" level
      VolumeBy level -> how "relative" level
      RelativeStep steps -> how "relative_step" steps
  where
    how :: ToJSON a => Text -> a -> [Pair]
    how name level = ["how" .= name, "value" .= level]

-- | Mute ('True') or unmute an output.
mute :: (Moo :> es, Error RoonError :> es) => OutputId -> Bool -> Eff es ()
mute outputId muted = call (method "mute") (object ["output_id" .= outputId, "how" .= muteName muted])

-- | Mute ('True') or unmute every output that can be.
muteAll :: (Moo :> es, Error RoonError :> es) => Bool -> Eff es ()
muteAll muted = call (method "mute_all") (object ["how" .= muteName muted])

muteName :: Bool -> Text
muteName muted = if muted then "mute" else "unmute"

-- | Move the queue of a zone to another zone.
transferZone
  :: (ZoneOrOutput from, ZoneOrOutput to, Moo :> es, Error RoonError :> es)
  => from
  -> to
  -> Eff es ()
transferZone from to =
  call (method "transfer_zone") $
    object ["from_zone_or_output_id" .= zoneOrOutputId from, "to_zone_or_output_id" .= zoneOrOutputId to]

method :: Text -> Text
method name = transportService <> "/" <> name
