module Data.Iter.MVar where

import Control.Concurrent.MVar

import Data.Iter

pushToMVar :: MVar a -> Iter a -> IO ()
pushToMVar v i = do
    a <- nextIO i
    case a of
        Nothing     -> return ()
        Just (x,i') -> do
            putMVar v x
            pushToMVar v i'

iterMVar :: MVar a -> Iter a
iterMVar v = IterIO $ do
    a <- takeMVar v
    return $ a ::: iterMVar v
