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
getNext = modifyIter next

withNext :: (a -> b) -> IState a b
withNext f = fmap f getNext

withNext2 :: (a -> a -> b) -> IState a b
withNext2 f = liftM2 f getNext getNext

modifyIter :: (Iter a -> Iter (b, Iter a)) -> IState a b
modifyIter f = do
    i <- get
    (b, i') <- liftIO $ ihead (f i)
    put i'
    return b

modifyIter_ :: (Iter a -> Iter a) -> IState a ()
modifyIter_ = modify

runIter :: IState a b -> Iter a -> Iter b
runIter s = IterIO . fmap (return.fst) . runIStateIO s

runIState :: IState a b -> Iter a -> Iter (b, Iter a)
runIState s = IterIO . fmap return . runIStateIO s

runIterIO :: IState a b -> Iter a -> IO b
runIterIO s = fmap fst . runIStateIO s

runIStateIO :: IState a b -> Iter a -> IO (b, Iter a)
runIStateIO s = runStateT (runI s)
