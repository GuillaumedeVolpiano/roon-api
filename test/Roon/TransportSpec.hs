{-# LANGUAGE RecordWildCards #-}

-- | The transport service, against a scripted server. The zone and output
-- fixtures are answers of a real Roon Server 2.75.
module Roon.TransportSpec (tests) where

import Data.Aeson (Value (..), decodeStrictText, object, (.=))
import Data.List (sort, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Text (Text)
import Roon.Effect.Moo (defaultRequestHandler, runMoo)
import Roon.Error (RoonError (..))
import Roon.Moo.Message
import Roon.Test.Server
import Roon.Transport
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream qualified as Stream
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  localOption (mkTimeout 5_000_000) $
    testGroup
      "Roon.Transport"
      [ testCase "decodes the zones of a real server" $ do
          (outcome, ()) <- withServer Map.empty (runMoo defaultRequestHandler getZones) $ \wire -> do
            question <- expectRequest wire "com.roonlabs.transport:2/get_zones"
            answer wire question Complete "Success" (JsonBody realZones)
          fmap fst outcome @?= Right [livingRoom]
      , testCase "decodes the outputs of a real server" $ do
          (outcome, ()) <- withServer Map.empty (runMoo defaultRequestHandler getOutputs) $ \wire -> do
            question <- expectRequest wire "com.roonlabs.transport:2/get_outputs"
            answer wire question Complete "Success" (JsonBody (object ["outputs" .= [realOutput]]))
          fmap fst outcome @?= Right [livingRoomOutput]
      , testCase "operates a zone" $ do
          (outcome, ()) <- withServer Map.empty (runMoo defaultRequestHandler (control livingRoom.zoneId Next)) $ \wire -> do
            question <- expectRequest wire "com.roonlabs.transport:2/control"
            field "zone_or_output_id" question @?= Just (String "zone-0001")
            field "control" question @?= Just (String "next")
            answer wire question Complete "Success" NoBody
          fmap fst outcome @?= Right ()
      , testCase "reports a refused control" $ do
          (outcome, ()) <- withServer Map.empty (runMoo defaultRequestHandler (control livingRoomOutput.outputId Play)) $ \wire -> do
            question <- expectRequest wire "com.roonlabs.transport:2/control"
            field "zone_or_output_id" question @?= Just (String "output-0001")
            answer wire question Complete "InvalidRequest" NoBody
          fmap fst outcome @?= Left (UnexpectedReply "com.roonlabs.transport:2/control" "InvalidRequest")
      , testCase "changes only the settings asked for" $ do
          let change = noSettingsChange {setShuffle = Just True, setLoop = Just NextLoop}
          (outcome, ()) <- withServer Map.empty (runMoo defaultRequestHandler (changeSettings livingRoom.zoneId change)) $ \wire -> do
            question <- expectRequest wire "com.roonlabs.transport:2/change_settings"
            question.body
              @?= JsonBody
                ( object
                    [ "zone_or_output_id" .= ("zone-0001" :: Text)
                    , "shuffle" .= True
                    , "loop" .= ("next" :: Text)
                    ]
                )
            answer wire question Complete "Success" NoBody
          fmap fst outcome @?= Right ()
      , testCase "follows the zones, then unsubscribes with the same key" $ do
          let client = runMoo defaultRequestHandler (subscribeZones (Stream.fold zones . Stream.take 3))
          (outcome, ()) <- withServer Map.empty client $ \wire -> do
            subscription <- expectRequest wire "com.roonlabs.transport:2/subscribe_zones"
            let key = field "subscription_key" subscription
            key @?= Just (Number 0)
            answer wire subscription Continue "Subscribed" (JsonBody realZones)
            answer wire subscription Continue "Changed" . JsonBody $
              object
                [ "zones_added" .= [object ["zone_id" .= ("kitchen" :: Text), "display_name" .= ("Kitchen" :: Text), "state" .= ("stopped" :: Text)]]
                , "zones_seek_changed" .= [object ["zone_id" .= livingRoom.zoneId, "seek_position" .= (100 :: Int), "queue_time_remaining" .= (2152 :: Int)]]
                ]
            answer wire subscription Continue "Changed" (JsonBody (object ["zones_removed" .= ["kitchen" :: Text]]))
            unsubscription <- expectRequest wire "com.roonlabs.transport:2/unsubscribe_zones"
            field "subscription_key" unsubscription @?= key
            answer wire unsubscription Complete "Success" NoBody
          let seeked = let Zone {..} = livingRoom in Zone {nowPlaying = atSeek 100 <$> nowPlaying, queueTimeRemaining = Just 2152, ..}
          fmap fst outcome @?= Right (Map.singleton livingRoom.zoneId seeked)
      , testCase "gives a view of the zones after each event" $ do
          views <- Stream.toList (zoneViews (Stream.fromList zoneScript))
          map Map.keys views @?= [[livingRoom.zoneId], [livingRoom.zoneId], [livingRoom.zoneId], [livingRoom.zoneId, kitchen], [livingRoom.zoneId, kitchen]]
      , testCase "holds back views that only move the seek position" $ do
          views <- Stream.toList (zoneViewsEvery 3600 (Stream.fromList zoneScript))
          -- The subscription, then the new zone, which carries the
          -- positions held back.
          map Map.keys views @?= [[livingRoom.zoneId], [livingRoom.zoneId, kitchen]]
          fmap (.nowPlaying) (Map.lookup livingRoom.zoneId (last views)) @?= Just (atSeek 120 <$> livingRoom.nowPlaying)
      , testCase "gives every view when the interval is zero" $ do
          views <- Stream.toList (zoneViewsEvery 0 (Stream.fromList zoneScript))
          length views @?= length zoneScript
      , testCase "ends the stream when the server ends the subscription" $ do
          let client = runMoo defaultRequestHandler (subscribeZones (Stream.fold Fold.length))
          (outcome, ()) <- withServer Map.empty client $ \wire -> do
            subscription <- expectRequest wire "com.roonlabs.transport:2/subscribe_zones"
            answer wire subscription Continue "Subscribed" (JsonBody realZones)
            answer wire subscription Complete "Unsubscribed" NoBody
            _ <- expectRequest wire "com.roonlabs.transport:2/unsubscribe_zones"
            pure ()
          fmap fst outcome @?= Right 1
      , testCase "reports an unexpected subscription status" $ do
          let client = runMoo defaultRequestHandler (subscribeZones (Stream.fold Fold.drain))
          (outcome, ()) <- withServer Map.empty client $ \wire -> do
            subscription <- expectRequest wire "com.roonlabs.transport:2/subscribe_zones"
            answer wire subscription Complete "NotAllowed" NoBody
            _ <- expectRequest wire "com.roonlabs.transport:2/unsubscribe_zones"
            pure ()
          fmap fst outcome @?= Left (UnexpectedReply "com.roonlabs.transport:2/subscribe_zones" "NotAllowed")
      , testCase "follows zones and queues as one stream" $ do
          let client = runMoo defaultRequestHandler (subscribeTransport [livingRoom.zoneId] 2 (Stream.toList . Stream.take 2))
          (outcome, ()) <- withServer Map.empty client $ \wire -> do
            zoneSubscription <- expectRequest wire "com.roonlabs.transport:2/subscribe_zones"
            queueSubscription <- expectRequest wire "com.roonlabs.transport:2/subscribe_queue"
            answer wire queueSubscription Continue "Subscribed" (JsonBody realQueue)
            answer wire zoneSubscription Continue "Subscribed" (JsonBody realZones)
            ends <- (,) <$> receiveFromClient wire <*> receiveFromClient wire
            let names = [(fst ends).name, (snd ends).name]
            sort names @?= ["com.roonlabs.transport:2/unsubscribe_queue", "com.roonlabs.transport:2/unsubscribe_zones"]
          fmap (sortOn show . fst) outcome
            @?= Right (sortOn show [QueueUpdate livingRoom.zoneId (QueueSubscribed [firstSong, secondSong]), ZoneUpdate (ZonesSubscribed [livingRoom])])
      , testCase "reads the first items of a queue" $ do
          let client = runMoo defaultRequestHandler (subscribeQueue livingRoom.zoneId 2 (Stream.fold Fold.one))
          (outcome, ()) <- withServer Map.empty client $ \wire -> do
            subscription <- expectRequest wire "com.roonlabs.transport:2/subscribe_queue"
            field "zone_or_output_id" subscription @?= Just (String "zone-0001")
            field "max_item_count" subscription @?= Just (Number 2)
            answer wire subscription Continue "Subscribed" (JsonBody realQueue)
            _ <- expectRequest wire "com.roonlabs.transport:2/unsubscribe_queue"
            pure ()
          fmap fst outcome @?= Right (Just (QueueSubscribed [firstSong, secondSong]))
      , testCase "follows a queue through a real change" $ do
          let client = runMoo defaultRequestHandler (subscribeQueue livingRoom.zoneId 5 (Stream.fold queue . Stream.take 2))
          (outcome, ()) <- withServer Map.empty client $ \wire -> do
            subscription <- expectRequest wire "com.roonlabs.transport:2/subscribe_queue"
            answer wire subscription Continue "Subscribed" (JsonBody capturedQueue)
            answer wire subscription Continue "Changed" (JsonBody capturedQueueChange)
            _ <- expectRequest wire "com.roonlabs.transport:2/unsubscribe_queue"
            pure ()
          fmap (map (.queueItemId) . fst) outcome @?= Right [1002, 1003, 1004, 1005, 1006]
      , testCase "keeps a queue operation it does not know" $
          applyQueueEvent (QueueChanged [OtherQueueChange (object ["operation" .= ("move" :: Text)])]) [firstSong]
            @?= [firstSong]
      ]

atSeek :: Int -> NowPlaying -> NowPlaying
atSeek position (NowPlaying _ len image artists one two three) = NowPlaying (Just position) len image artists one two three

kitchen :: ZoneId
kitchen = ZoneId "zone-0002"

-- | A subscription, two seek moves, a new zone, another seek move.
zoneScript :: [ZoneEvent]
zoneScript =
  [ ZonesSubscribed [livingRoom]
  , seekTo 110
  , seekTo 120
  , ZonesChanged noChanges {added = [kitchenZone]}
  , seekTo 130
  ]
  where
    seekTo position = ZonesChanged noChanges {seekChanged = [SeekChange {zoneId = livingRoom.zoneId, seekPosition = Just position, queueTimeRemaining = Nothing}]}
    noChanges = ZoneChanges {removed = [], added = [], changed = [], seekChanged = []}
    kitchenZone = let Zone {..} = livingRoom in Zone {zoneId = kitchen, displayName = "Kitchen", outputs = [], nowPlaying = Nothing, ..}

-- Fixtures

realZones :: Value
realZones = fromJust . decodeStrictText $
  "{\"zones\":[{\"display_name\":\"Living Room\",\"is_next_allowed\":true,\"is_pause_allowed\":false,\"is_play_allowed\":true,\
  \\"is_previous_allowed\":true,\"is_seek_allowed\":true,\"now_playing\":{\"artist_image_keys\":[\"artist-image-0001\"],\
  \\"image_key\":\"album-image-0001\",\"length\":306,\"one_line\":{\"line1\":\"First Song - The Example Band\"},\
  \\"seek_position\":82,\"three_line\":{\"line1\":\"First Song\",\"line2\":\"The Example Band\",\"line3\":\"Example Album\"},\
  \\"two_line\":{\"line1\":\"First Song\",\"line2\":\"The Example Band\"}},\
  \\"outputs\":[{\"can_group_with_output_ids\":[\"output-0001\"],\"display_name\":\"Living Room\",\
  \\"output_id\":\"output-0001\",\
  \\"source_controls\":[{\"control_key\":\"1\",\"display_name\":\"Amplifier\",\"status\":\"indeterminate\",\"supports_standby\":false}],\
  \\"zone_id\":\"zone-0001\"}],\
  \\"queue_items_remaining\":11,\"queue_time_remaining\":2170,\
  \\"settings\":{\"auto_radio\":true,\"loop\":\"disabled\",\"shuffle\":false},\"state\":\"paused\",\"zone_id\":\"zone-0001\"}]}"

realOutputText :: Text
realOutputText =
  "{\"can_group_with_output_ids\":[\"output-0001\"],\"display_name\":\"Living Room\",\
  \\"output_id\":\"output-0001\",\
  \\"source_controls\":[{\"control_key\":\"1\",\"display_name\":\"Amplifier\",\"status\":\"indeterminate\",\"supports_standby\":false}],\
  \\"zone_id\":\"zone-0001\"}"

realOutput :: Value
realOutput = fromJust (decodeStrictText realOutputText)

realQueue :: Value
realQueue = fromJust . decodeStrictText $
  "{\"items\":[{\"image_key\":\"album-image-0001\",\"length\":306,\"one_line\":{\"line1\":\"First Song - The Example Band\"},\
  \\"queue_item_id\":1001,\"three_line\":{\"line1\":\"First Song\",\"line2\":\"The Example Band\",\"line3\":\"Example Album\"},\
  \\"two_line\":{\"line1\":\"First Song\",\"line2\":\"The Example Band\"}},\
  \{\"image_key\":\"album-image-0001\",\"length\":159,\"one_line\":{\"line1\":\"Second Song - The Example Band\"},\
  \\"queue_item_id\":1002,\"three_line\":{\"line1\":\"Second Song\",\"line2\":\"The Example Band\",\"line3\":\"Example Album\"},\
  \\"two_line\":{\"line1\":\"Second Song\",\"line2\":\"The Example Band\"}}]}"

-- | A queue of five items, as Roon reported it while Living Room played.
capturedQueue :: Value
capturedQueue = fromJust . decodeStrictText $
  "{\"items\":[{\"image_key\":\"album-image-0001\",\"length\":306,\"one_line\":{\"line1\":\"First Song - The Example Band\"},\"queue_item_id\":1001,\"three_line\":{\"line1\":\"First Song\",\"line2\":\"The Example Band\",\"line3\":\"Example Album\"},\"two_line\":{\"line1\":\"First Song\",\"line2\":\"The Example Band\"}},\
  \  {\"image_key\":\"album-image-0001\",\"length\":159,\"one_line\":{\"line1\":\"Second Song - The Example Band\"},\"queue_item_id\":1002,\"three_line\":{\"line1\":\"Second Song\",\"line2\":\"The Example Band\",\"line3\":\"Example Album\"},\"two_line\":{\"line1\":\"Second Song\",\"line2\":\"The Example Band\"}},\
  \  {\"image_key\":\"album-image-0001\",\"length\":179,\"one_line\":{\"line1\":\"Third Song - The Example Band\"},\"queue_item_id\":1003,\"three_line\":{\"line1\":\"Third Song\",\"line2\":\"The Example Band\",\"line3\":\"Example Album\"},\"two_line\":{\"line1\":\"Third Song\",\"line2\":\"The Example Band\"}},\
  \  {\"image_key\":\"album-image-0001\",\"length\":242,\"one_line\":{\"line1\":\"Fourth Song - The Example Band\"},\"queue_item_id\":1004,\"three_line\":{\"line1\":\"Fourth Song\",\"line2\":\"The Example Band\",\"line3\":\"Example Album\"},\"two_line\":{\"line1\":\"Fourth Song\",\"line2\":\"The Example Band\"}},\
  \  {\"image_key\":\"album-image-0001\",\"length\":137,\"one_line\":{\"line1\":\"Fifth Song - The Example Band\"},\"queue_item_id\":1005,\"three_line\":{\"line1\":\"Fifth Song\",\"line2\":\"The Example Band\",\"line3\":\"Example Album\"},\"two_line\":{\"line1\":\"Fifth Song\",\"line2\":\"The Example Band\"}}]}"

-- | What Roon sent for that queue when the first track ended.
capturedQueueChange :: Value
capturedQueueChange = fromJust . decodeStrictText $
  "{\"changes\":[{\"count\":5,\"index\":0,\"operation\":\"remove\"},{\"index\":0,\"items\":[{\"image_key\":\"album-image-0001\",\"length\":159,\"one_line\":{\"line1\":\"Second Song - The Example Band\"},\"queue_item_id\":1002,\"three_line\":{\"line1\":\"Second Song\",\"line2\":\"The Example Band\",\"line3\":\"Example Album\"},\"two_line\":{\"line1\":\"Second Song\",\"line2\":\"The Example Band\"}},\
  \  {\"image_key\":\"album-image-0001\",\"length\":179,\"one_line\":{\"line1\":\"Third Song - The Example Band\"},\"queue_item_id\":1003,\"three_line\":{\"line1\":\"Third Song\",\"line2\":\"The Example Band\",\"line3\":\"Example Album\"},\"two_line\":{\"line1\":\"Third Song\",\"line2\":\"The Example Band\"}},\
  \  {\"image_key\":\"album-image-0001\",\"length\":242,\"one_line\":{\"line1\":\"Fourth Song - The Example Band\"},\"queue_item_id\":1004,\"three_line\":{\"line1\":\"Fourth Song\",\"line2\":\"The Example Band\",\"line3\":\"Example Album\"},\"two_line\":{\"line1\":\"Fourth Song\",\"line2\":\"The Example Band\"}},\
  \  {\"image_key\":\"album-image-0001\",\"length\":137,\"one_line\":{\"line1\":\"Fifth Song - The Example Band\"},\"queue_item_id\":1005,\"three_line\":{\"line1\":\"Fifth Song\",\"line2\":\"The Example Band\",\"line3\":\"Example Album\"},\"two_line\":{\"line1\":\"Fifth Song\",\"line2\":\"The Example Band\"}},\
  \  {\"image_key\":\"album-image-0001\",\"length\":181,\"one_line\":{\"line1\":\"Sixth Song - The Example Band\"},\"queue_item_id\":1006,\"three_line\":{\"line1\":\"Sixth Song\",\"line2\":\"The Example Band\",\"line3\":\"Example Album\"},\"two_line\":{\"line1\":\"Sixth Song\",\"line2\":\"The Example Band\"}}],\"operation\":\"insert\"}]}"

livingRoom :: Zone
livingRoom =
  Zone
    { zoneId = ZoneId "zone-0001"
    , displayName = "Living Room"
    , outputs = [livingRoomOutput]
    , state = Paused
    , isPreviousAllowed = True
    , isNextAllowed = True
    , isPauseAllowed = False
    , isPlayAllowed = True
    , isSeekAllowed = True
    , queueItemsRemaining = Just 11
    , queueTimeRemaining = Just 2170
    , settings = Just ZoneSettings {loop = LoopDisabled, shuffle = False, autoRadio = True}
    , nowPlaying =
        Just
          NowPlaying
            { seekPosition = Just 82
            , length = Just 306
            , imageKey = Just "album-image-0001"
            , artistImageKeys = ["artist-image-0001"]
            , oneLine = DisplayLines "First Song - The Example Band" Nothing Nothing
            , twoLine = DisplayLines "First Song" (Just "The Example Band") Nothing
            , threeLine = DisplayLines "First Song" (Just "The Example Band") (Just "Example Album")
            }
    }

livingRoomOutput :: Output
livingRoomOutput =
  Output
    { outputId = OutputId "output-0001"
    , zoneId = ZoneId "zone-0001"
    , displayName = "Living Room"
    , volume = Nothing
    , sourceControls = [SourceControl {controlKey = "1", displayName = "Amplifier", status = Indeterminate, supportsStandby = False}]
    , canGroupWithOutputIds = [OutputId "output-0001"]
    }

firstSong :: QueueItem
firstSong =
  QueueItem
    { queueItemId = 1001
    , length = Just 306
    , imageKey = Just "album-image-0001"
    , oneLine = DisplayLines "First Song - The Example Band" Nothing Nothing
    , twoLine = DisplayLines "First Song" (Just "The Example Band") Nothing
    , threeLine = DisplayLines "First Song" (Just "The Example Band") (Just "Example Album")
    }

secondSong :: QueueItem
secondSong =
  QueueItem
    { queueItemId = 1002
    , length = Just 159
    , imageKey = Just "album-image-0001"
    , oneLine = DisplayLines "Second Song - The Example Band" Nothing Nothing
    , twoLine = DisplayLines "Second Song" (Just "The Example Band") Nothing
    , threeLine = DisplayLines "Second Song" (Just "The Example Band") (Just "Example Album")
    }
