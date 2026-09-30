{-# LANGUAGE OverloadedStrings #-}

{- | Streaming codec contracts, abstract codec construction and bounded body
helpers. This module has no compression-implementation dependencies.
-}
module Network.Wai.Middleware.Compression.Codec (
    Codec,
    mkCodec,
    mkAdaptiveCodec,
    mkBufferedCodec,
    codecName,
    codecEncode,
    codecEncodeWith,
    codecDecode,
    Producer,
    Encoder,
    Decoder,
    CompressionProfile (..),
    CompressionError (..),
    BodyLimits (..),
    defaultBodyLimits,
    validateBodyLimits,
    encodeBody,
    encodeBodyBounded,
    decodeBody,
    decodeBodyWithSize,
    splitChunks,
    isToken,
) where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless, when)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Word (Word64)
import Network.Wai.Middleware.Compression.Buffer (newCollector)
import System.Timeout (timeout)

-- | Produce body chunks and explicit flush events through the supplied callbacks.
type Producer = (BS.ByteString -> IO ()) -> IO () -> IO ()

{- | Consume a producer, emit encoded chunks and forward flush events. Finish the
encoded stream after the producer returns; release resources on all exits.
-}
type Encoder = Producer -> (BS.ByteString -> IO ()) -> IO () -> IO ()

{- | Pull encoded chunks until an empty chunk signals EOF, emitting decoded
chunks. Validate termination and consume the complete input. Implementations
must release resources when a quota callback or asynchronous exception fails.
-}
type Decoder = IO BS.ByteString -> (BS.ByteString -> IO ()) -> IO ()

{- | Relative online-serving trade-offs: lower CPU, balanced cost/size, or a
stronger size bias. Adaptive adapters choose levels; fixed codecs ignore this.
-}
data CompressionProfile = Fast | Balanced | Compact
    deriving (Eq, Ord, Show, Enum, Bounded)

{- | An HTTP content-coding name with encoding and decoding implementations.
Construct through @mkCodec@, @mkAdaptiveCodec@ or @mkBufferedCodec@. Streaming
implementations must allocate per-operation state for concurrent codec reuse.
-}
data Codec = Codec !BS.ByteString (CompressionProfile -> Encoder) Decoder

-- | Lowercase HTTP wire name, such as @gzip@, @br@ or @zstd@.
codecName :: Codec -> BS.ByteString
codecName (Codec name _ _) = name

-- | Select the codec's Balanced encoder. Fixed-level codecs retain their level.
codecEncode :: Codec -> Encoder
codecEncode = codecEncodeWith Balanced

{- | Select an adaptive profile without changing the codec or decoder. Codecs
created with @mkCodec@ or @mkBufferedCodec@ ignore the requested profile.
-}
codecEncodeWith :: CompressionProfile -> Codec -> Encoder
codecEncodeWith profile (Codec _ encoder _) = encoder profile

{- | Access the streaming decoder. Direct callers must enforce their own byte,
expansion and time limits; use @decodeBody@ for the bounded convenience API.
-}
codecDecode :: Codec -> Decoder
codecDecode (Codec _ _ decoder) = decoder

instance Show Codec where
    show codec = "Codec " <> show (codecName codec)

{- | Configuration, malformed-input and resource-limit failures. Middleware maps
request-body failures to HTTP responses; direct body helpers throw exceptions.
Cancellation is not a malformed-body error and must propagate independently.
-}
data CompressionError
    = InvalidConfiguration String
    | InvalidCompressedBody String
    | EncodedBodyTooLarge
    | DecodedBodyTooLarge
    | ExpansionRatioExceeded
    | TooManyEncodingLayers
    | UnsupportedContentEncoding BS.ByteString
    | MalformedContentEncoding
    | BodyTimeout
    deriving (Eq, Show)

instance Exception CompressionError

{- | Per-body limits. Byte counts use @Integer@ to avoid arithmetic overflow.
Decoded output is limited by both @maxDecodedBytes@ and
@originalEncodedBytes * maxExpansionRatio + expansionRatioSlack@.
@maxEncodingLayers@ bounds stacked codings; @bodyTimeoutMicros@ is a best-effort
deadline, not a mechanism for forcibly preempting arbitrary unsafe FFI calls.
-}
data BodyLimits = BodyLimits
    { maxEncodedBytes :: !Integer
    , maxDecodedBytes :: !Integer
    , maxExpansionRatio :: !Integer
    , expansionRatioSlack :: !Integer
    , maxEncodingLayers :: !Int
    , bodyTimeoutMicros :: !Int
    }
    deriving (Eq, Show)

{- | 10 MiB encoded input, 32 MiB decoded output, expansion ratio 100 with 1 MiB
slack, at most three encoding layers, and a five-second deadline.
-}
defaultBodyLimits :: BodyLimits
defaultBodyLimits =
    BodyLimits
        { maxEncodedBytes = 10 * 1024 * 1024
        , maxDecodedBytes = 32 * 1024 * 1024
        , maxExpansionRatio = 100
        , expansionRatioSlack = 1024 * 1024
        , maxEncodingLayers = 3
        , bodyTimeoutMicros = 5 * 1000 * 1000
        }

{- | Reject nonpositive limits, negative expansion slack and decoded limits that
cannot be represented by WAI's @Word64@ request-length metadata.
-}
validateBodyLimits :: BodyLimits -> Either CompressionError ()
validateBodyLimits limits
    | maxEncodedBytes limits <= 0 = invalid "maxEncodedBytes must be positive"
    | maxDecodedBytes limits <= 0 = invalid "maxDecodedBytes must be positive"
    | maxDecodedBytes limits > toInteger (maxBound :: Word64) = invalid "maxDecodedBytes exceeds Word64"
    | maxExpansionRatio limits <= 0 = invalid "maxExpansionRatio must be positive"
    | expansionRatioSlack limits < 0 = invalid "expansionRatioSlack must not be negative"
    | maxEncodingLayers limits <= 0 = invalid "maxEncodingLayers must be positive"
    | bodyTimeoutMicros limits <= 0 = invalid "bodyTimeoutMicros must be positive"
    | otherwise = Right ()
  where
    invalid = Left . InvalidConfiguration

{- | Construct a fixed encoder/decoder pair. The name must be a lowercase HTTP
token other than @identity@ or @*@. Profiles do not change this encoder.
-}
mkCodec :: BS.ByteString -> Encoder -> Decoder -> Either CompressionError Codec
mkCodec name encoder = mkAdaptiveCodec name (const encoder)

{- | Construct a profile-dependent encoder with one decoder. The same lowercase
HTTP-token validation as @mkCodec@ applies to its name.
-}
mkAdaptiveCodec :: BS.ByteString -> (CompressionProfile -> Encoder) -> Decoder -> Either CompressionError Codec
mkAdaptiveCodec name encoder decoder
    | not (isToken name) || BS.any (\byte -> byte >= 65 && byte <= 90) name = invalid
    | name == "identity" || name == "*" = invalid
    | otherwise = Right (Codec name encoder decoder)
  where
    invalid = Left (InvalidConfiguration "codec name must be a lowercase HTTP token other than identity or *")

{- | Adapt pure strict-byte transforms for small bodies with a positive byte
limit. Input is checked while collecting; output is checked after the pure
transform returns. This cannot prevent allocations inside that transform and
is not a decompression-bomb sandbox. Intermediate flushes are coalesced.
-}
mkBufferedCodec :: Integer -> BS.ByteString -> (BS.ByteString -> BS.ByteString) -> (BS.ByteString -> BS.ByteString) -> Either CompressionError Codec
mkBufferedCodec limit name encoder decoder
    | limit <= 0 = Left (InvalidConfiguration "buffered codec limit must be positive")
    | otherwise = mkCodec name encode decode
  where
    transform function chunks emit = do
        let result = function (BS.concat (reverse chunks))
        when (toInteger (BS.length result) > limit) (throwIO DecodedBodyTooLarge)
        unless (BS.null result) (emit result)
    collect size chunks chunk = do
        let total = size + toInteger (BS.length chunk)
        when (total > limit) (throwIO EncodedBodyTooLarge)
        pure (total, BS.copy chunk : chunks)
    encode :: Encoder
    encode producer emit flush = do
        buffer <- newIORef (0, [])
        producer
            ( \chunk -> do
                (size, chunks) <- readIORef buffer
                updated <- collect size chunks chunk
                writeIORef buffer updated
            )
            (pure ())
        (_, chunks) <- readIORef buffer
        transform encoder chunks emit
        flush
    decode :: Decoder
    decode pull emit = loop 0 []
      where
        loop size chunks = do
            chunk <- pull
            if BS.null chunk
                then transform decoder chunks emit
                else do
                    (total, updated) <- collect size chunks chunk
                    loop total updated

{- | Test for a nonempty ASCII HTTP token. Codec constructors additionally reject
uppercase names and the reserved @identity@ and @*@ names.
-}
isToken :: BS.ByteString -> Bool
isToken bytes = not (BS.null bytes) && BS.all allowed bytes
  where
    allowed byte =
        (byte >= 48 && byte <= 57)
            || (byte >= 65 && byte <= 90)
            || (byte >= 97 && byte <= 122)
            || BS.elem byte "!#$%&'*+-.^_`|~"

{- | Split strict bytes into chunks of at most 32 KiB. Empty input produces no
chunks; splitting does not copy the underlying bytes.
-}
splitChunks :: BS.ByteString -> [BS.ByteString]
splitChunks bytes
    | BS.null bytes = []
    | otherwise = prefix : splitChunks suffix
  where
    (prefix, suffix) = BS.splitAt 32768 bytes

{- | Encode and collect a lazy body using Balanced. No size limit is imposed;
use @encodeBodyBounded@ when collected output must have a byte limit.
-}
encodeBody :: Codec -> LBS.ByteString -> IO LBS.ByteString
encodeBody = encodeCollect (const (pure ()))

{- | Encode while bounding collected output bytes. This does not bound input or
native allocations. Exceeding the output limit throws @DecodedBodyTooLarge@,
the collector's output-quota error; callers decide its HTTP interpretation.
-}
encodeBodyBounded :: Integer -> Codec -> LBS.ByteString -> IO LBS.ByteString
encodeBodyBounded limit = encodeCollect (\size -> when (size > limit) (throwIO DecodedBodyTooLarge))

encodeCollect :: (Integer -> IO ()) -> Codec -> LBS.ByteString -> IO LBS.ByteString
encodeCollect check codec body = do
    (emit, finish) <- newCollector check
    codecEncode
        codec
        (\send _ -> mapM_ send (concatMap splitChunks (LBS.toChunks body)))
        emit
        (pure ())
    finish

{- | Decode with validated input/output/expansion limits and a deadline. Input
counting stops at its quota. Return only after complete decoding and input
validation; malformed data and exceeded limits throw @CompressionError@.
-}
decodeBody :: BodyLimits -> Codec -> LBS.ByteString -> IO LBS.ByteString
decodeBody limits codec body = do
    either throwIO pure (validateBodyLimits limits)
    outcome <- timeout (bodyTimeoutMicros limits) $ do
        let measure total [] = pure total
            measure total (chunk : chunks) = do
                let size = total + toInteger (BS.length chunk)
                when (size > maxEncodedBytes limits) (throwIO EncodedBodyTooLarge)
                measure size chunks
        size <- measure 0 (LBS.toChunks body)
        decodeBodyWithSize limits size codec body
    maybe (throwIO BodyTimeout) pure outcome

{- | Decode using an already validated original encoded size for expansion
accounting. Intended for bounded stacked-layer decoding: this helper does not
count the supplied body's input length or install a deadline. Callers must
enforce those boundaries independently; otherwise use @decodeBody@.
-}
decodeBodyWithSize :: BodyLimits -> Integer -> Codec -> LBS.ByteString -> IO LBS.ByteString
decodeBodyWithSize limits originalSize codec body = do
    either throwIO pure (validateBodyLimits limits)
    when (originalSize < 0 || originalSize > maxEncodedBytes limits) (throwIO EncodedBodyTooLarge)
    source <- newIORef (concatMap splitChunks (LBS.toChunks body))
    (emit, finish) <- newCollector $ \total -> do
        when (total > maxDecodedBytes limits) (throwIO DecodedBodyTooLarge)
        when
            (total > originalSize * maxExpansionRatio limits + expansionRatioSlack limits)
            (throwIO ExpansionRatioExceeded)
    let pull = do
            remaining <- readIORef source
            case remaining of
                [] -> pure BS.empty
                chunk : rest -> writeIORef source rest >> pure chunk
    codecDecode codec pull emit
    remaining <- pull
    unless (BS.null remaining) (throwIO (InvalidCompressedBody "unconsumed input"))
    finish
