{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Iter.State where

import Data.Iter
import Data.Functor
import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans

newtype IState a b = IState { runI :: StateT (Iter a) IO b }
    deriving (Functor, Applicative, Monad, MonadIO, MonadState (Iter a))


getNext :: IState a a
getNext = do
    i <- get
    n <- liftIO $ nextIO i
    case n of
        Nothing -> error "iteration exhausted"
        Just (a, i') -> do
            put i'
            return a

modifyIter :: (Iter a -> Iter (b, Iter a)) -> IState a b
modifyIter = undefined

modifyIter_ :: (Iter a -> Iter b) -> IState a ()
modifyIter_ = undefined


runIter :: IState a b -> Iter a -> IO b
runIter s = fmap fst . runIState s

runIState :: IState a b -> Iter a -> IO (b, Iter a)
runIState s = runStateT (runI s)
