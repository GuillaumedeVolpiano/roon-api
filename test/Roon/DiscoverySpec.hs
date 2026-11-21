-- | SOOD: the format, and a discovery against a fake server on loopback.
module Roon.DiscoverySpec (tests) where

import Control.Concurrent.Async (withAsync)
import Control.Exception (bracket)
import Control.Monad (void)
import Data.ByteString qualified as ByteString
import Effectful (runEff)
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as SocketBytes
import Roon.Discovery
import Roon.Effect.WebSocket (ServerAddress (..))
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  localOption (mkTimeout 10_000_000) $
    testGroup
      "Roon.Discovery"
      [ testCase "encodes a query" $
          encodeSood SoodMessage {kind = 'Q', properties = [("a", Just "bc"), ("d", Nothing)]}
            @?= "SOOD\2Q\1a\0\2bc\1d\255\255"
      , testCase "decodes what it encodes" $ do
          let message = SoodMessage {kind = 'R', properties = [("name", Just "Test Core"), ("empty", Just ""), ("none", Nothing)]}
          decodeSood (encodeSood message) @?= Just message
      , testCase "rejects what is not SOOD version 2" $ do
          decodeSood "SOOP\2Q" @?= Nothing
          decodeSood "SOOD\1Q" @?= Nothing
      , testCase "rejects a datagram cut inside a property" $ do
          let name = ("name", Just "Test Core")
              first = SoodMessage {kind = 'R', properties = [name]}
              full = encodeSood SoodMessage {kind = 'R', properties = [name, ("port", Just "9330")]}
              boundary = ByteString.length (encodeSood first)
          -- Every prefix that ends inside a property is refused; the one that
          -- ends between the two properties is the first one alone.
          [decodeSood (ByteString.take n full) | n <- [7 .. ByteString.length full - 1], n /= boundary]
            @?= replicate (ByteString.length full - 8) Nothing
          decodeSood (ByteString.take boundary full) @?= Just first
      , testCase "reads a server from an answer" $
          fmap (\core -> (core.uniqueId, core.name, core.address)) (coreFromAnswer "192.0.2.7" (answer []))
            @?= Just ("core-1", Just "Test Core", ServerAddress {host = "192.0.2.7", port = 9330})
      , testCase "uses the reply address of the answer" $
          fmap (.address) (coreFromAnswer "192.0.2.7" (answer [("_replyaddr", Just "192.0.2.8")]))
            @?= Just ServerAddress {host = "192.0.2.8", port = 9330}
      , testCase "ignores answers for other services" $
          coreFromAnswer "192.0.2.7" SoodMessage {kind = 'R', properties = [("service_id", Just "other"), ("unique_id", Just "x"), ("http_port", Just "1")]}
            @?= Nothing
      , testCase "finds a server that answers, once" $
          withFakeServer $ \port -> do
            cores <- runEff (discover DiscoveryOptions {targets = [(loopback, port)], wait = 300_000})
            map (\core -> (core.uniqueId, core.address)) cores @?= [("core-1", ServerAddress {host = "127.0.0.1", port = 9330})]
      , testCase "finds it again: each query has a transaction id of its own" $
          withFakeServer $ \port -> do
            let options = DiscoveryOptions {targets = [(loopback, port)], wait = 300_000}
            first <- runEff (discover options)
            second <- runEff (discover options)
            (length first, length second) @?= (1, 1)
      ]
  where
    answer extra =
      SoodMessage
        { kind = 'R'
        , properties =
            [ ("name", Just "Test Core")
            , ("unique_id", Just "core-1")
            , ("service_id", Just roonServiceId)
            , ("http_port", Just "9330")
            ]
              <> extra
        }

loopback :: Socket.HostAddress
loopback = Socket.tupleToHostAddress (127, 0, 0, 1)

-- | A server on a free loopback port that answers every query three
-- times, as Roon answers several times, and, like Roon, ignores a query
-- with the transaction id of the previous one, or without any.
withFakeServer :: (Socket.PortNumber -> IO a) -> IO a
withFakeServer run =
  bracket (Socket.socket Socket.AF_INET Socket.Datagram Socket.defaultProtocol) Socket.close $ \socket -> do
    Socket.bind socket (Socket.SockAddrInet 0 loopback)
    port <- Socket.socketPort socket
    let reply =
          encodeSood
            SoodMessage
              { kind = 'R'
              , properties = [("unique_id", Just "core-1"), ("service_id", Just roonServiceId), ("http_port", Just "9330")]
              }
        serve previous = do
          (query, sender) <- SocketBytes.recvFrom socket 65535
          case decodeSood query of
            Just SoodMessage {kind = 'Q', properties}
              | Just (Just transaction) <- lookup "_tid" properties
              , Just transaction /= previous -> do
                  mapM_ (const (void (SocketBytes.sendTo socket reply sender))) [1 .. 3 :: Int]
                  serve (Just transaction)
            _ -> serve previous
    withAsync (serve Nothing) (const (run port))
