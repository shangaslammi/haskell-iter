
module Data.Iter.String where

import Data.Iter
import Data.Char

import Control.Monad
import Control.Exception

import System.IO
import System.IO.Error

type IString = Iter Char

ilines' :: IString -> Iter String
ilines' = isplitWhen' (=='\n')

iwords' :: IString -> Iter String
iwords' = iconcatMap listify . iwords

ilines :: IString -> Iter IString
ilines = isplitWhen (=='\n')

iwords :: IString -> Iter IString
iwords = ifEmpty (StopIteration) $ \i -> do
    (j,k) <- ibreak (isSpace) $ idropWhile (isSpace) i
    let rest = iwords k
    ifEmpty rest (\j -> j !:: iwords k) j

iunwords :: Iter IString -> IString
iunwords = iintercalate " "

iterFile :: FilePath -> Iter Char
iterFile fp = IterIO $ do
    fh <- openFile fp ReadMode
    return $ iterHandle fh

iterHandle :: Handle -> Iter Char
iterHandle fh = Finalize (closeHandle) $ IterIO $ nextChar fh where
    closeHandle = hClose fh
    nextChar fh = do
        e <- tryJust (guard . isEOFError) (hGetChar fh)
        case e of
            Left  _ -> do
                hClose fh
                return StopIteration
            Right c -> return $ c ::: IterIO (nextChar fh)

