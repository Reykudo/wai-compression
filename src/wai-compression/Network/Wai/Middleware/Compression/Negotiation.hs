{-# LANGUAGE OverloadedStrings #-}

{- | Strict @Accept-Encoding@ parsing, integer quality values and stable
negotiation. Server preference resolves equal qualities only.
-}
module Network.Wai.Middleware.Compression.Negotiation (
    NegotiationError (..),
    negotiateEncoding,
    encodingQuality,
    addAcceptEncodingVary,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.List qualified as List
import Network.HTTP.Types.Header (Header, hAcceptEncoding, hVary)
import Network.Wai.Middleware.Compression.Codec (isToken)

-- | A malformed header or the absence of an acceptable coding, including identity.
data NegotiationError = InvalidAcceptEncoding | NoAcceptableEncoding
    deriving (Eq, Show)

{- | Select a server-registered coding in preference order. @Nothing@ represents
identity. Omitted qualities are 1000; @q=0@ excludes a coding. Malformed headers
and complete exclusion return an error rather than a guessed encoding.
-}
negotiateEncoding :: [BS.ByteString] -> [Header] -> Either NegotiationError (Maybe BS.ByteString)
negotiateEncoding preference headers = do
    parsed <- parseAcceptEncoding headers
    let options =
            [(Just coding, quality parsed coding) | coding <- preference]
                <> [(Nothing, quality parsed "identity")]
        choose best candidate = if snd candidate > snd best then candidate else best
        selected = List.foldl' choose (Nothing, 0) options
    if snd selected == 0 then Left NoAcceptableEncoding else Right (fst selected)

{- | Parse a coding's quality as an integer from 0 to 1000, including wildcard
and identity rules. Duplicate entries use the most restrictive quality.
-}
encodingQuality :: BS.ByteString -> [Header] -> Either NegotiationError Int
encodingQuality name headers = (`quality` lower name) <$> parseAcceptEncoding headers

parseAcceptEncoding :: [Header] -> Either NegotiationError (Maybe [(BS.ByteString, Int)])
parseAcceptEncoding headers
    | sum (map BS.length values) > 8192 = Left InvalidAcceptEncoding
    | null values = Right Nothing
    | otherwise = Right (Just (map parseEntry (concatMap (BSC.split ',') values)))
  where
    values = [value | (name, value) <- headers, name == hAcceptEncoding]
    parseEntry value = case map trim (BSC.split ';' value) of
        [name] | isToken name -> (lower name, 1000)
        [name, parameter] | isToken name -> (lower name, parseParameter parameter)
        name : _ -> (lower name, 0)
        [] -> (BS.empty, 0)
    parseParameter parameter = case BSC.split '=' parameter of
        [key, value] | lower (trim key) == "q" -> parseQuality (trim value)
        _ -> 0

parseQuality :: BS.ByteString -> Int
parseQuality value = case BSC.split '.' value of
    [whole] | whole == "0" -> 0
    [whole] | whole == "1" -> 1000
    [whole, fractional]
        | BS.length fractional <= 3 && BS.all (\byte -> byte >= 48 && byte <= 57) fractional ->
            if whole == "1" && BS.all (== 48) fractional
                then 1000
                else
                    if whole == "0"
                        then digits (fractional <> BS.replicate (3 - BS.length fractional) 48)
                        else 0
    _ -> 0
  where
    digits = BS.foldl' (\total byte -> total * 10 + fromIntegral byte - 48) 0

quality :: Maybe [(BS.ByteString, Int)] -> BS.ByteString -> Int
quality Nothing name = if name == "identity" then 1000 else 0
quality (Just entries) name = case [weight | (coding, weight) <- entries, coding == name] of
    weights@(_ : _) -> minimum weights
    [] | name == "identity" -> if wildcard == Just 0 then 0 else 1000
    [] -> maybe 0 id wildcard
  where
    wildcard = case [weight | (coding, weight) <- entries, coding == "*"] of
        [] -> Nothing
        weights -> Just (minimum weights)

{- | Add @Accept-Encoding@ to @Vary@ without duplicating an existing token or
changing an existing wildcard. Preserve unrelated response headers.
-}
addAcceptEncodingVary :: [Header] -> [Header]
addAcceptEncodingVary headers
    | any (\value -> lower value == "accept-encoding" || value == "*") tokens = headers
    | otherwise =
        (hVary, BSC.intercalate ", " (tokens <> ["Accept-Encoding"]))
            : filter ((/= hVary) . fst) headers
  where
    tokens = filter (not . BS.null) [trim token | (name, value) <- headers, name == hVary, token <- BSC.split ',' value]

lower :: BS.ByteString -> BS.ByteString
lower = BSC.map (\character -> if character >= 'A' && character <= 'Z' then toEnum (fromEnum character + 32) else character)

trim :: BS.ByteString -> BS.ByteString
trim = BSC.dropWhileEnd space . BSC.dropWhile space
  where
    space character = character == ' ' || character == '\t'
