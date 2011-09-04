module Data.Iter.MVar where

import Control.Concurrent.MVar

import Data.Iter

pushToMVar :: MVar a -> Iter a -> IO ()
pushToMVar = undefined

iterMVar :: MVar a -> Iter a
iterMVar = undefined
