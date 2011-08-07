
module Data.Iter.ByteString where

import Data.Iter
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
        if B.null chunk
            then StopIteration
            else return $ chunk :: IterIO (nextChunk h)


iwriteFile :: FilePath -> IByteString -> IO ()
iwriteFile = undefined

iwriteHandle :: Handle -> IByteString -> IO ()
iwriteHandle = undefined
