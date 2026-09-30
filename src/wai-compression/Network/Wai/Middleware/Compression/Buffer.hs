module Network.Wai.Middleware.Compression.Buffer (newCollector) where

import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (newIORef, readIORef, writeIORef)

newCollector :: (Integer -> IO ()) -> IO (BS.ByteString -> IO (), IO LBS.ByteString)
newCollector check = do
    state <- newIORef (0, 0, [], [])
    let emit chunk = unless (BS.null chunk) $ do
            (size, pendingSize, pending, complete) <- readIORef state
            let !total = size + toInteger (BS.length chunk)
            check total
            (nextSize, nextPending, nextComplete) <- add chunk pendingSize pending complete
            writeIORef state (total, nextSize, nextPending, nextComplete)
        add chunk pendingSize pending complete
            | BS.null chunk = pure (pendingSize, pending, complete)
            | otherwise = do
                let capacity = 32768 - pendingSize
                    (prefix, suffix) = BS.splitAt capacity chunk
                    newSize = pendingSize + BS.length prefix
                    parts = prefix : pending
                if newSize == 32768
                    then
                        add suffix 0 [] (BS.concat (reverse parts) : complete)
                    else pure (newSize, BS.copy prefix : pending, complete)
        finish = do
            (_, pendingSize, pending, complete) <- readIORef state
            let chunks = if pendingSize == 0 then complete else BS.concat (reverse pending) : complete
            pure (LBS.fromChunks (reverse chunks))
    pure (emit, finish)
