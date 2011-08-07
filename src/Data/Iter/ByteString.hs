
module Data.Iter.ByteString where

import Data.Iter
import Data.Word
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import System.IO

type IByteString = Iter ByteString

chunkSize = 1024*8

iterFile :: FilePath -> IByteString
iterFile fp = IterIO $ fmap (iterHandle) $ openBinaryFile fp ReadMode

iterHandle :: Handle -> IByteString
iterHandle h = Finalize (closeHandle) $ IterIO $ nextChunk h where
    closeHandle = hClose h
    nextChunk h = do
        chunk <- B.hGetSome h chunkSize
        return $ if B.null chunk
            then StopIteration
            else chunk ::: IterIO (nextChunk h)


iwriteFile :: FilePath -> IByteString -> IO ()
iwriteFile fp = withBinaryFile fp WriteMode . flip iwriteHandle

iwriteHandle :: Handle -> IByteString -> IO ()
iwriteHandle h i = do
    n <- nextIO i
    case n of
        Nothing          -> return ()
        Just (chunk, i') -> B.hPut h chunk >> iwriteHandle h i'

iterBytes :: ByteString -> Iter Word8
iterBytes = iterList . B.unpack

bytes :: IByteString -> Iter Word8
bytes i = do
    (chunk, i') <- next i
    iterBytes chunk +++ bytes i'

toByteString :: IByteString -> IO ByteString
toByteString = fmap B.concat . toList
