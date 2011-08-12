
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

takeBytes :: Int -> IByteString -> IByteString
takeBytes 0 _ = StopIteration
takeBytes n i = do
    (chunk, i') <- next i
    let l = B.length chunk
    if l < n
        then chunk !:: takeBytes (n-l) i'
        else return $ B.take n chunk

dropBytes :: Int -> IByteString -> IByteString
dropBytes 0 i = i
dropBytes n i = do
    (chunk, i') <- next i
    let l = B.length chunk
    if l < n
        then dropBytes (n-l) i'
        else B.drop n chunk !:: i'

dropBytesWhile :: (Word8 -> Bool) -> IByteString -> IByteString
dropBytesWhile f i = do
    (chunk, i') <- next i
    let c = B.dropWhile f chunk
    if B.null c
        then dropBytesWhile f i'
        else c !:: i'

takeBytesWhile :: (Word8 -> Bool) -> IByteString -> IByteString
takeBytesWhile f i = do
    (chunk, i') <- next i
    let c = B.takeWhile f chunk
    if B.length c == B.length chunk
        then chunk !:: takeBytesWhile f i'
        else return c

splitBytesAt :: Int -> IByteString -> Iter (IByteString, IByteString)
splitBytesAt 0 i = return (StopIteration, i)
splitBytesAt n i = do
    p <- peek i
    case p of
        Nothing -> return (StopIteration, StopIteration)
        Just (chunk, i') -> do
            let l = B.length chunk
            if l < n
                then do
                    (a,b) <- splitBytesAt (n-l) i'
                    return (chunk !:: a, b)
                else do
                    let (a,b) = B.splitAt n chunk
                    return (return a, b !:: i')

