-- |
-- Module      : Roon.Image
-- Description : The images of a Roon Server, fetched over HTTP
-- License     : BSD-3-Clause
--
-- Zones, queue items and browse items carry image keys. A Roon Server
-- serves the image of a key over HTTP, on the port of its extension API:
-- @http:\/\/host:port\/api\/image\/KEY@, with the size wanted in the query.
-- No registration and no token are needed.
--
-- Observed with Roon Server 2.75: an unknown key answers 404; an invalid
-- option answers 400 with a line of text; without options the image comes
-- in its stored size; @image\/png@ was asked and @image\/jpeg@ came back,
-- so the content type to trust is the one of the answer.
module Roon.Image
  ( -- * Options
    ImageOptions (..)
  , ImageSize (..)
  , Scale (..)
  , ImageFormat (..)
  , originalImage
  , fitWithin
  , imageTarget

    -- * Fetching
  , Image (..)
  , ImageLimits (..)
  , defaultImageLimits
  , fetchImage
  , fetchImageWithin
  ) where

import Control.Exception (IOException, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Effectful
import Effectful.Error.Static (Error, throwError)
import Roon.Effect.WebSocket (ServerAddress (..))
import Roon.Error (RoonError (..))
import Streamly.Data.Scanl qualified as Scanl
import Streamly.Data.Stream qualified as Stream
import Streamly.External.ByteString qualified as StreamlyBS
import Streamly.External.Network.HTTP qualified as Http
import System.Timeout (timeout)

-- | How Roon fits an image into the size asked.
data Scale
  = -- | The whole image inside the size, its proportions kept.
    Fit
  | -- | The size covered, its proportions kept, what exceeds cut off.
    Fill
  | -- | Exactly the size, proportions given up.
    Stretch
  deriving stock (Eq, Show)

-- | The size of the image wanted.
data ImageSize
  = -- | As stored by Roon.
    Original
  | -- | Scaled to a width and a height in pixels.
    Scaled Scale Int Int
  deriving stock (Eq, Show)

-- | The format to ask for.
data ImageFormat
  = Jpeg
  | Png
  deriving stock (Eq, Show)

-- | What to ask for. 'Nothing' as the format leaves the choice to Roon.
data ImageOptions = ImageOptions
  { size :: ImageSize
  , format :: Maybe ImageFormat
  }
  deriving stock (Eq, Show)

-- | The image as stored, in the format Roon chooses.
originalImage :: ImageOptions
originalImage = ImageOptions {size = Original, format = Nothing}

-- | The whole image within a square of that many pixels, in the format
-- Roon chooses.
fitWithin :: Int -> ImageOptions
fitWithin side = ImageOptions {size = Scaled Fit side side, format = Nothing}

-- | The path and the query that ask a Roon Server for an image.
imageTarget :: Text -> ImageOptions -> ByteString
imageTarget key options =
  "/api/image/" <> percentEncode (encodeUtf8 key) <> case sizeFields <> formatFields of
    [] -> ""
    fields -> "?" <> ByteString.intercalate "&" [name <> "=" <> value | (name, value) <- fields]
  where
    sizeFields = case options.size of
      Original -> []
      Scaled scale width height ->
        [("scale", scaleName scale), ("width", Char8.pack (show width)), ("height", Char8.pack (show height))]
    formatFields = case options.format of
      Nothing -> []
      Just Jpeg -> [("format", "image/jpeg")]
      Just Png -> [("format", "image/png")]
    scaleName = \case
      Fit -> "fit"
      Fill -> "fill"
      Stretch -> "stretch"

-- | Percent-encode every byte that is not unreserved (RFC 3986).
percentEncode :: ByteString -> ByteString
percentEncode = LazyByteString.toStrict . Builder.toLazyByteString . ByteString.foldr (\byte rest -> encode byte <> rest) mempty
  where
    encode byte
      | unreserved byte = Builder.word8 byte
      | otherwise = Builder.char7 '%' <> hexDigit (byte `div` 16) <> hexDigit (byte `mod` 16)
    hexDigit digit = Builder.word8 (ByteString.index "0123456789ABCDEF" (fromIntegral digit))
    unreserved byte =
      (byte >= 48 && byte <= 57)
        || (byte >= 65 && byte <= 90)
        || (byte >= 97 && byte <= 122)
        || ByteString.elem byte "-._~"

-- | An image as served by Roon.
data Image = Image
  { contentType :: Text
  -- ^ For example @image\/jpeg@. Empty when the server names none.
  , bytes :: ByteString
  }
  deriving stock (Eq, Show)

-- | The bounds of a fetch.
data ImageLimits = ImageLimits
  { microseconds :: Int
  -- ^ For the whole exchange.
  , maxBytes :: Int
  -- ^ For the image.
  }
  deriving stock (Eq, Show)

-- | Thirty seconds and 32 MiB.
defaultImageLimits :: ImageLimits
defaultImageLimits = ImageLimits {microseconds = 30_000_000, maxBytes = 32 * 1024 * 1024}

-- | Fetch the image of a key from a Roon Server, within
-- 'defaultImageLimits'.
--
-- Throws 'ImageFailed' when the server refuses (unknown key, invalid
-- option), answers something that is not HTTP, or sends too large an
-- image; 'ConnectFailed' when the connection fails; 'TimedOut' when the
-- exchange takes too long.
fetchImage :: (IOE :> es, Error RoonError :> es) => ServerAddress -> Text -> ImageOptions -> Eff es Image
fetchImage = fetchImageWithin defaultImageLimits

-- | Like 'fetchImage', within other limits.
fetchImageWithin :: (IOE :> es, Error RoonError :> es) => ImageLimits -> ServerAddress -> Text -> ImageOptions -> Eff es Image
fetchImageWithin limits address key options = do
  outcome <- liftIO . try . try . timeout limits.microseconds $ Http.withResponse request receive
  case outcome of
    Left (problem :: IOException) -> throwError (ConnectFailed (Text.pack (show problem)))
    Right (Left (problem :: Http.HttpError)) -> throwError (ImageFailed (Text.pack (show problem)))
    Right (Right Nothing) -> throwError (TimedOut "image")
    Right (Right (Just (Left refusal))) -> throwError (ImageFailed refusal)
    Right (Right (Just (Right image))) -> pure image
  where
    request = Http.get address.host address.port (imageTarget key options)
    receive response
      | response.status /= 200 = do
          -- Roon explains a refusal in a line of text.
          (_, explanation) <- collect 1024 response.body
          pure . Left . Text.strip . Text.unwords $
            [Text.pack (show response.status), decodeUtf8Lenient response.reason, decodeUtf8Lenient (ByteString.take 1024 explanation)]
      | otherwise = do
          (complete, content) <- collect limits.maxBytes response.body
          pure $
            if complete
              then Right Image {contentType = maybe "" decodeUtf8Lenient (Http.header "content-type" response), bytes = content}
              else Left ("the image is larger than " <> Text.pack (show limits.maxBytes) <> " bytes")
    -- Read the body until it ends or exceeds a number of bytes: whether it
    -- ended within the limit, and what was read.
    collect limit body = do
      -- Each array with the number of bytes read before it.
      let counted = Stream.postscanl (Scanl.mkScanl (\(before, previous) array -> (before + ByteString.length previous, array)) (0, "")) (fmap StreamlyBS.fromArray body)
      chunks <- Stream.toList (fmap snd (Stream.takeWhile (\(before, _) -> before <= limit) counted))
      let content = ByteString.concat chunks
      pure (ByteString.length content <= limit, content)
