{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Iter.State where

import Data.Iter
import Control.Monad.State
import Control.Monad.Trans

newtype IState a b = IState { runI :: StateT (Iter a) IO b }
    deriving (Monad, MonadIO, MonadState (Iter a))


getNext :: IState a a
getNext = do
    i <- get
    n <- liftIO $ nextIO i
    case n of
        Nothing -> error "iteration exhausted"
        Just (a, i') -> do
            put i'
            return a

runIter :: IState a b -> Iter a -> IO b
runIter s = fmap fst . runIState s

runIState :: IState a b -> Iter a -> IO (b, Iter a)
runIState s = runStateT (runI s)
