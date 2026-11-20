module Main (main) where

import Roon.BrowseSpec qualified as BrowseSpec
import Roon.ConnectionSpec qualified as ConnectionSpec
import Roon.Effect.TokenStoreSpec qualified as TokenStoreSpec
import Roon.Effect.WebSocketSpec qualified as WebSocketSpec
import Roon.Moo.MessageSpec qualified as MessageSpec
import Roon.TransportSpec qualified as TransportSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "roon-api"
      [ MessageSpec.tests
      , TokenStoreSpec.tests
      , ConnectionSpec.tests
      , WebSocketSpec.tests
      , TransportSpec.tests
      , BrowseSpec.tests
      ]
