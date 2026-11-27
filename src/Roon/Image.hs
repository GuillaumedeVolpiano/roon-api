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
-- The request goes through the t'Http' effect of @effectful-streamly-http@:
-- 'runImageHttp' reaches the server, and tests answer from a function.
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
  , fetchImage

    -- * Reaching the server
  , runImageHttp
  , ImageLimits (..)
  , defaultImageLimits
  ) where

import Data.ByteString (ByteString, index, intercalate)
import Data.ByteString qualified as BS (elem, foldr, take)
import Data.ByteString.Builder (char7, toLazyByteString, word8)
import Data.ByteString.Char8 qualified as BS8 (pack)
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text, strip)
import Data.Text qualified as T (pack, unwords)
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Effectful (Eff, IOE, inject, type (:>))
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.HTTP (Http, HttpError (..), HttpFailure, Limits (..),
                       Response (..), Scheme (..), defaultLimits, fetch, get,
                       header, runHttpClear)
import Effectful.HTTP qualified as H (HttpFailure (ConnectFailed, RequestFailed))
import Roon.Effect.WebSocket (ServerAddress (..))
import Roon.Error (RoonError (..))

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
    fields -> "?" <> intercalate "&" [name <> "=" <> value | (name, value) <- fields]
  where
    sizeFields = case options.size of
      Original -> []
      Scaled scale width height ->
        [("scale", scaleName scale), ("width", BS8.pack (show width)), ("height", BS8.pack (show height))]
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
percentEncode = toStrict . toLazyByteString . BS.foldr (\byte rest -> encode byte <> rest) mempty
  where
    encode byte
      | unreserved byte = word8 byte
      | otherwise = char7 '%' <> hexDigit (byte `div` 16) <> hexDigit (byte `mod` 16)
    hexDigit digit = word8 (index "0123456789ABCDEF" (fromIntegral digit))
    unreserved byte =
      (byte >= 48 && byte <= 57)
        || (byte >= 65 && byte <= 90)
        || (byte >= 97 && byte <= 122)
        || BS.elem byte "-._~"

-- | An image as served by Roon.
data Image = Image
  { contentType :: Text
  -- ^ For example @image\/jpeg@. Empty when the server names none.
  , bytes :: ByteString
  }
  deriving stock (Eq, Show)

-- | Fetch the image of a key from a Roon Server.
--
-- Throws 'ImageFailed' when the server refuses (unknown key, invalid
-- option). What the connection itself can do wrong is up to the
-- interpreter of t'Http': see 'runImageHttp'.
fetchImage :: (Http :> es, Error RoonError :> es) => ServerAddress -> Text -> ImageOptions -> Eff es Image
fetchImage address key options = do
  response <- fetch Http (get address.host address.port (imageTarget key options))
  if response.status /= 200
    then
      -- Roon explains a refusal in a line of text.
      throwError . ImageFailed . strip . T.unwords $
        [T.pack (show response.status), decodeUtf8Lenient response.reason, decodeUtf8Lenient (BS.take 1024 response.body)]
    else pure Image {contentType = maybe "" decodeUtf8Lenient (header "content-type" response), bytes = response.body}

-- | The bounds of a fetch.
data ImageLimits = ImageLimits
  { microseconds :: Int
  -- ^ For each wait: for the connection to open, and for the server to go
  -- on once it is.
  , maxBytes :: Int
  -- ^ For the image.
  }
  deriving stock (Eq, Show)

-- | Thirty seconds and 32 MiB.
defaultImageLimits :: ImageLimits
defaultImageLimits = ImageLimits {microseconds = 30_000_000, maxBytes = 32 * 1024 * 1024}

-- | Reach the Roon Server for its images: in clear, one connection for
-- each image, within limits.
--
-- Throws 'ConnectFailed' when the connection fails, 'TimedOut' when the
-- server takes too long, and 'ImageFailed' when it answers something that
-- is not HTTP or sends too large an image.
runImageHttp :: (IOE :> es, Error RoonError :> es) => ImageLimits -> Eff (Http : es) a -> Eff es a
runImageHttp limits action =
  runErrorNoCallStack (runHttpClear bounds (inject action)) >>= either (throwError . failure) pure
  where
    bounds =
      defaultLimits
        { connectTimeout = Just limits.microseconds
        , idleTimeout = Just limits.microseconds
        , maxBodySize = Just limits.maxBytes
        }
    failure :: HttpFailure -> RoonError
    failure = \case
      H.ConnectFailed cause -> ConnectFailed cause
      H.RequestFailed ConnectTimedOut -> TimedOut "image"
      H.RequestFailed ResponseTimedOut -> TimedOut "image"
      H.RequestFailed (BodyTooLarge limit) -> ImageFailed ("the image is larger than " <> T.pack (show limit) <> " bytes")
      H.RequestFailed problem -> ImageFailed (T.pack (show problem))
      other -> ImageFailed (T.pack (show other))
