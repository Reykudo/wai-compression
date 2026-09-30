{-# LANGUAGE OverloadedStrings #-}

{- | Optional server preferences based on response MIME and already-known size.
Policies do not inspect body bytes and cannot override client quality values.
-}
module Network.Wai.Middleware.Compression.Policy (
    EncodingPlan (..),
    EncodingPolicy,
    ResponseInfo (..),
    responseInfo,
    defaultEncodingPolicy,
    recommendedPolicy,
    recommendedPolicyFor,
    validateEncodingPlan,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.List (sort, sortOn)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Request, Response, responseHeaders)
import Network.Wai.Middleware.Compression.Codec (CompressionError (..), CompressionProfile (..))
import Network.Wai.Middleware.Compression.Encoding qualified as Encoding
import Network.Wai.Middleware.Compression.Internal (contentLength, lower, trim)

{- | A permutation of registered codec names and the profile to use if encoding
is selected. Every available name must occur exactly once.
-}
data EncodingPlan = EncodingPlan
    { preferredEncodings :: [BS.ByteString]
    , compressionProfile :: CompressionProfile
    }
    deriving (Eq, Show)

{- | Choose server order and profile from the request, response and registered
names. The order only resolves equal client qualities; @q=0@ stays excluded.
-}
type EncodingPolicy = Request -> Response -> [BS.ByteString] -> EncodingPlan

{- | Optional content type and uncompressed byte count. A known size is an
advisory application hint, not a reason to traverse a lazy body to count it.
-}
data ResponseInfo = ResponseInfo
    { responseMimeType :: Maybe BS.ByteString
    , responseBodySize :: Maybe Integer
    }
    deriving (Eq, Show)

{- | Read only response headers. Duplicate, invalid or longer-than-20-digit
@Content-Length@ values produce an unknown size. The body is never evaluated.
-}
responseInfo :: Response -> ResponseInfo
responseInfo response = ResponseInfo (lookup hContentType headers) (contentLength headers)
  where
    headers = responseHeaders response

{- | Preserve registration order and choose Balanced. This is the core default;
the MIME/size recommendation is opt-in.
-}
defaultEncodingPolicy :: EncodingPolicy
defaultEncodingPolicy _ _ available = EncodingPlan available Balanced

{- | Apply @recommendedPolicyFor@ to header-only @responseInfo@ metadata. This
does not enable response compression or replace application safety checks.
-}
recommendedPolicy :: EncodingPolicy
recommendedPolicy _ response = recommendedPolicyFor (responseInfo response)

{- | Prefer Brotli for markup with a known size of at least 16 KiB; prefer zstd
for smaller/unknown markup, JSON and text. Other types preserve order. Use Fast
for unknown or smaller sizes, Balanced otherwise. Skip unavailable algorithms
and preserve custom-codec order. The thresholds are heuristic, not an optimum.
-}
recommendedPolicyFor :: ResponseInfo -> [BS.ByteString] -> EncodingPlan
recommendedPolicyFor information available = EncodingPlan (sortOn rank available) profile
  where
    mime = maybe BS.empty (lower . trim . BSC.takeWhile (/= ';')) (responseMimeType information)
    small = maybe True (< 16384) (responseBodySize information)
    profile = if small then Fast else Balanced
    markup = mime `elem` ["text/html", "text/css", "application/javascript", "text/javascript", "application/xml", "application/xhtml+xml", "image/svg+xml"] || "+xml" `BS.isSuffixOf` mime
    textual = "text/" `BS.isPrefixOf` mime || mime == "application/json" || "+json" `BS.isSuffixOf` mime
    preference
        | markup && not small = [Encoding.brotli, Encoding.zstd, Encoding.gzip, Encoding.deflate]
        | markup || textual = [Encoding.zstd, Encoding.brotli, Encoding.gzip, Encoding.deflate]
        | otherwise = []
    rank name = case [position | (position, coding) <- zip [0 :: Int ..] preference, coding == name] of
        position : _ -> position
        [] -> length preference

-- | Reject an order that is not an exact permutation of the registered names.
validateEncodingPlan :: [BS.ByteString] -> EncodingPlan -> Either CompressionError EncodingPlan
validateEncodingPlan available plan
    | sort available == sort (preferredEncodings plan) = Right plan
    | otherwise = Left (InvalidConfiguration "encoding policy must return each registered codec exactly once")
