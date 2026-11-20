{-# LANGUAGE MultilineStrings #-}

-- | The browse service, against a scripted server. The load fixtures are
-- answers of a real Roon Server 2.75 to a search, without image keys.
module Roon.BrowseSpec (tests) where

import Control.Monad (replicateM_)
import Data.Aeson (Value (..), decodeStrictText, object, (.=))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Text (Text)
import Roon.Browse
import Roon.Effect.Moo (defaultRequestHandler, runMoo)
import Roon.Error (RoonError (..))
import Roon.Moo.Message
import Roon.Test.Server
import Roon.Transport (ZoneId (..))
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  localOption (mkTimeout 5_000_000) $
    testGroup
      "Roon.Browse"
      [ testCase "searches for tracks in a session" $ do
          let searchSession = Session Search (Just "s")
              client = runMoo defaultRequestHandler (searchTracks defaultSearchOptions searchSession "The Example Band Second Song")
          (outcome, ()) <- withServer Map.empty client $ \wire -> do
            search <- expectRequest wire "com.roonlabs.browse:1/browse"
            search.body
              @?= JsonBody
                ( object
                    [ "hierarchy" .= ("search" :: Text)
                    , "multi_session_key" .= ("s" :: Text)
                    , "pop_all" .= True
                    , "input" .= ("The Example Band Second Song" :: Text)
                    ]
                )
            answer wire search Complete "Success" listAction
            top <- expectRequest wire "com.roonlabs.browse:1/load"
            field "multi_session_key" top @?= Just (String "s")
            answer wire top Complete "Success" (JsonBody searchLevel)
            open <- expectRequest wire "com.roonlabs.browse:1/browse"
            field "item_key" open @?= Just (String "1:1")
            answer wire open Complete "Success" listAction
            tracks <- expectRequest wire "com.roonlabs.browse:1/load"
            field "count" tracks @?= Just (Number 10)
            answer wire tracks Complete "Success" (JsonBody tracksLevel)
          fmap (map (\candidate -> (candidate.item.title, candidate.item.itemKey, candidate.session)) . fst) outcome
            @?= Right [("Second Song", Just "2:0", searchSession)]
      , testCase "finds no track without a category of tracks" $ do
          let client = runMoo defaultRequestHandler (searchTracks defaultSearchOptions (session Search) "nothing")
          (outcome, ()) <- withServer Map.empty client $ \wire -> do
            search <- expectRequest wire "com.roonlabs.browse:1/browse"
            answer wire search Complete "Success" listAction
            top <- expectRequest wire "com.roonlabs.browse:1/load"
            answer wire top Complete "Success" . JsonBody $
              object ["list" .= object ["title" .= ("Search" :: Text), "count" .= (0 :: Int), "level" .= (0 :: Int)], "offset" .= (0 :: Int), "items" .= ([] :: [Value])]
          fmap fst outcome @?= Right []
      , testCase "opens a track down to its actions and queues it" $ do
          let client = runMoo defaultRequestHandler (performTrackAction livingRoom (trackActionTitle Queue) secondSong)
          (outcome, ()) <- withServer Map.empty client $ \wire -> do
            openTrack <- expectRequest wire "com.roonlabs.browse:1/browse"
            (field "item_key" openTrack, field "zone_or_output_id" openTrack) @?= (Just (String "2:0"), Just (String "zone-1"))
            answer wire openTrack Complete "Success" listAction
            trackLevel' <- expectRequest wire "com.roonlabs.browse:1/load"
            answer wire trackLevel' Complete "Success" (JsonBody trackLevel)
            openActions <- expectRequest wire "com.roonlabs.browse:1/browse"
            field "item_key" openActions @?= Just (String "3:0")
            answer wire openActions Complete "Success" listAction
            actions <- expectRequest wire "com.roonlabs.browse:1/load"
            answer wire actions Complete "Success" (JsonBody actionLevel)
            queue <- expectRequest wire "com.roonlabs.browse:1/browse"
            (field "item_key" queue, field "zone_or_output_id" queue) @?= (Just (String "4:2"), Just (String "zone-1"))
            -- What Roon Server 2.75 answered: it goes back to the level of
            -- the track.
            answer wire queue Complete "Success" (JsonBody queuedReply)
          fmap fst outcome @?= Right ()
      , testCase "reports an error message from Roon" $ do
          let client = runMoo defaultRequestHandler (performTrackAction livingRoom (trackActionTitle PlayNow) secondSong)
          (outcome, ()) <- withServer Map.empty client $ \wire -> do
            serveActions wire
            play <- expectRequest wire "com.roonlabs.browse:1/browse"
            field "item_key" play @?= Just (String "4:0")
            answer wire play Complete "Success" . JsonBody $
              object ["action" .= ("message" :: Text), "message" .= ("Zone is not available" :: Text), "is_error" .= True]
          fmap fst outcome @?= Left (BrowseFailed "Zone is not available")
      , testCase "reports an action that is not offered" $ do
          let client = runMoo defaultRequestHandler (performTrackAction livingRoom "Shuffle" secondSong)
          (outcome, ()) <- withServer Map.empty client serveActions
          fmap fst outcome @?= Left (BrowseItemMissing "Shuffle for Second Song")
      , testCase "searches concurrently and reports each query" $ do
          let client = runMoo defaultRequestHandler (searchTracksConcurrently defaultSearchOptions "q-" ["Second Song", "broken"])
          (outcome, ()) <- withServer Map.empty client $ \wire ->
            -- Four requests for the first search, one for the second, in
            -- any order: answer by session.
            replicateM_ 5 $ do
              message <- receiveFromClient wire
              case (field "multi_session_key" message, message.name, field "item_key" message) of
                (Just (String "q-1"), _, _) -> answer wire message Complete "InvalidRequest" NoBody
                (_, "com.roonlabs.browse:1/browse", _) -> answer wire message Complete "Success" listAction
                (_, _, _) -> do
                  -- The first load reads the search, the second the tracks.
                  let level = if field "count" message == Just (Number 100) then searchLevel else tracksLevel
                  answer wire message Complete "Success" (JsonBody level)
          fmap (map (fmap (fmap (map (.item.title)))) . fst) outcome
            @?= Right [("Second Song", Right ["Second Song"]), ("broken", Left (UnexpectedReply "com.roonlabs.browse:1/browse" "InvalidRequest"))]
      ]

livingRoom :: ZoneId
livingRoom = ZoneId "zone-1"

secondSong :: Candidate
secondSong =
  Candidate
    { session = Session Search (Just "s")
    , item = Item {title = "Second Song", subtitle = Nothing, imageKey = Nothing, itemKey = Just "2:0", hint = Just ActionList, inputPrompt = Nothing}
    }

-- | Answer the four requests that lead from 'secondSong' to its actions.
serveActions :: Wire -> IO ()
serveActions wire = do
  openTrack <- expectRequest wire "com.roonlabs.browse:1/browse"
  answer wire openTrack Complete "Success" listAction
  trackLevel' <- expectRequest wire "com.roonlabs.browse:1/load"
  answer wire trackLevel' Complete "Success" (JsonBody trackLevel)
  openActions <- expectRequest wire "com.roonlabs.browse:1/browse"
  answer wire openActions Complete "Success" listAction
  actions <- expectRequest wire "com.roonlabs.browse:1/load"
  answer wire actions Complete "Success" (JsonBody actionLevel)

-- | What browse answers when it moves to a list; its content does not
-- matter to the client.
listAction :: Body
listAction =
  JsonBody . object $
    ["action" .= ("list" :: Text), "list" .= object ["title" .= ("Search" :: Text), "count" .= (2 :: Int), "level" .= (0 :: Int)]]

-- Real answers

searchLevel :: Value
searchLevel = fromJust . decodeStrictText $
  """
  {"items":[{"hint":"action_list","item_key":"1:0","subtitle":"The Example Band, Ann Writer, Bob Writer","title":"Second Song"},
  {"hint":"list","item_key":"1:1","subtitle":"1 Result","title":"Tracks"}],
  "list":{"count":2,"display_offset":null,"level":0,"subtitle":"The Example Band Second Song","title":"Search"},"offset":0}
  """

tracksLevel :: Value
tracksLevel = fromJust . decodeStrictText $
  """
  {"items":[{"hint":"action_list","item_key":"2:0","subtitle":"The Example Band, Ann Writer, Bob Writer","title":"Second Song"}],
  "list":{"count":1,"display_offset":null,"level":1,"subtitle":"1 Result","title":"Tracks"},"offset":0}
  """

trackLevel :: Value
trackLevel = fromJust . decodeStrictText $
  """
  {"items":[{"hint":"action_list","item_key":"3:0","subtitle":"The Example Band, Ann Writer, Bob Writer","title":"Second Song"}],
  "list":{"count":1,"display_offset":null,"level":2,"subtitle":"The Example Band, Ann Writer, Bob Writer","title":"Second Song"},"offset":0}
  """

actionLevel :: Value
actionLevel = fromJust . decodeStrictText $
  """
  {"items":[{"hint":"action","item_key":"4:0","subtitle":null,"title":"Play Now"},{"hint":"action","item_key":"4:1","subtitle":null,"title":"Add Next"},
  {"hint":"action","item_key":"4:2","subtitle":null,"title":"Queue"},{"hint":"action","item_key":"4:3","subtitle":null,"title":"Start Radio"}],
  "list":{"count":4,"display_offset":null,"hint":"action_list","level":3,"subtitle":"The Example Band, Ann Writer, Bob Writer","title":"Second Song"},"offset":0}
  """

queuedReply :: Value
queuedReply = fromJust . decodeStrictText $
  """
  {"action":"list","list":{"count":1,"display_offset":null,"image_key":"album-image-0001","level":2,
  "subtitle":"The Example Band, Ann Writer, Bob Writer","title":"Second Song"}}
  """
