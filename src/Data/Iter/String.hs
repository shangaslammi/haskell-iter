
module Data.Iter.String where

import Data.Iter

import Control.Monad
import Control.Exception

import System.IO
import System.IO.Error

ilines :: Iter Char -> Iter String
ilines = undefined

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

