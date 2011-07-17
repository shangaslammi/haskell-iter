module Data.Iter where

import Data.Maybe (fromJust)
import Data.Monoid (Monoid(..))

import Control.Applicative (Applicative(..))
import Control.Monad
import Control.Monad.IO.Class

data Iter a
    = IterIO (IO (Iter a))
    | StopIteration
    | a ::: Iter a
    | Iter a ::~ a


instance Functor Iter where
    fmap _ StopIteration = StopIteration
    fmap f (a ::: i)     = f a ::: fmap f i
    fmap f (i ::~ a)     = fmap f i ::~ f a
    fmap f (IterIO io)   = IterIO $ io >>= return . fmap f

instance Applicative Iter where
    pure a  = (a ::: StopIteration)

    StopIteration <*> _ = StopIteration
    _ <*> StopIteration = StopIteration
    (f ::: i) <*> j     = fmap f j +++ (i <*> j)
    (i ::~ f) <*> j     = (i <*> j) +++ fmap f j
    (IterIO io) <*> j   = IterIO $ io >>= (return . (<*> j))

instance Monad Iter where
    return a = (a ::: StopIteration)

    StopIteration >>= _ = StopIteration
    (a ::: i)     >>= f = f a +++ (i >>= f)
    (i ::~ a)     >>= f = (i >>= f) +++ (f a)
    (IterIO io)   >>= f = IterIO $ io >>= (return . (>>= f))

instance MonadIO Iter where
    liftIO io = IterIO $ do
        a <- io
        return (a ::: StopIteration)

instance MonadPlus Iter where
    mzero = StopIteration
    mplus = (+++)

instance Monoid (Iter a) where
    mempty  = StopIteration
    mappend = (+++)

returnIO :: (a, Iter a) -> IO (Maybe (a, Iter a))
returnIO = return . return

nextIO :: Iter a -> IO (Maybe (a, Iter a))
nextIO (IterIO io)    = io >>= nextIO
nextIO StopIteration  = return Nothing
nextIO (a ::: i)      = returnIO (a,i)

nextIO (StopIteration ::~ a) = returnIO (a, StopIteration)
nextIO (i ::~ a) = do
    n <- nextIO i
    case n of
        Nothing -> returnIO (a, StopIteration)
        Just x  -> returnIO x

next :: Iter a -> Iter (a, Iter a)
next StopIteration = StopIteration
next (a ::: i)     = return (a, i)
next (i ::~ a)     = do
    (x,i') <- next i
    return (x, i' ::~ a)
next (IterIO io)   = IterIO $ io >>= (return . next)

(+++) :: Iter a -> Iter a -> Iter a
StopIteration +++ j = j
i +++ StopIteration = i
(a ::: i) +++ j     = a ::: (i +++ j)
(i ::~ a) +++ j     = i +++ (a ::: j)
i +++ j = IterIO $ do
    ni <- nextIO i
    case ni of
        Nothing     -> return j
        Just (a,i') -> return $ a ::: (i' +++ j)

----- List-like operations for iterators
imap :: (a -> b) -> Iter a -> Iter b
imap = fmap

iconcat :: Iter (Iter a) -> Iter a
iconcat i = do
    (a, i') <- next i
    a +++ iconcat i'

iconcatMap :: (a -> Iter b) -> Iter a -> Iter b
iconcatMap f = iconcat . imap f

ifilter :: (a -> Bool) -> Iter a -> Iter a
ifilter f i = do
    x <- i
    guard (f x)
    return x

ifoldr :: (a -> b -> b) -> b -> Iter a -> Iter b
ifoldr _ acc StopIteration = return acc
ifoldr f acc (a ::: i)     = ifoldr f acc i >>= (return . f a)
ifoldr f acc (i ::~ a)     = acc `seq` ifoldr f (f a acc) i
ifoldr f acc (IterIO io)   = liftIO $ io >>= ifoldrIO f acc

ifoldl :: (b -> a -> b) -> b -> Iter a -> Iter b
ifoldl _ acc StopIteration = return acc
ifoldl f acc (a ::: i)     = acc `seq` ifoldl f (f acc a) i
ifoldl f acc (i ::~ a)     = do
    acc' <- ifoldl f acc i
    return $ f acc' a
ifoldl f acc (IterIO io)   = liftIO $ io >>= ifoldlIO f acc

izip :: Iter a -> Iter b -> Iter (a,b)
izip StopIteration _     = StopIteration
izip _ StopIteration     = StopIteration
izip (a ::: i) (b ::: j) = (a,b) ::: izip i j
izip i j = do
    (a, i') <- next i
    (b, j') <- next j
    (a,b) ::: izip i' j'

izipWith :: (a -> b -> c) -> Iter a -> Iter b -> Iter c
izipWith f i = imap (uncurry f) . izip i

iunfold :: (Iter a -> Iter (b, Iter a)) -> Iter a -> Iter b
iunfold f i = do
    (b, i') <- f i
    b ::: iunfold f i'

----- Evaluate fold results in the IO Monad -----
ifoldrIO :: (a -> b -> b) -> b -> Iter a -> IO b
ifoldrIO f acc i = liftM (fst.fromJust) $ nextIO (ifoldr f acc i)

ifoldlIO :: (b -> a -> b) -> b -> Iter a -> IO b
ifoldlIO f acc i = liftM (fst.fromJust) $ nextIO (ifoldl f acc i)

----- Conversion from and to lists -----
iterList :: [a] -> Iter a
iterList []     = StopIteration
iterList (x:xs) = x ::: iterList xs

toList :: Iter a -> IO [a]
toList StopIteration = return []
toList (a ::: i)     = toList i >>= return . (a:)
toList (i ::~ a)     = toList i >>= return . (++[a])

----- Sequence IO actions from iterators -----
ifor :: Iter a -> (a -> IO b) -> IO [b]
ifor i f = isequence $ fmap f i

isequence :: Iter (IO a) -> IO [a]
isequence StopIteration = return []
isequence (a ::: i)     = do
    x  <- a
    xs <- isequence i
    return (x:xs)
isequence (i ::~ a)     = do
    xs <- isequence i
    x  <- a
    return (xs ++ [x])
isequence (IterIO io)   = io >>= isequence

isequence_ :: Iter (IO a) -> IO ()
isequence_ = join . ifoldrIO (>>) (return ())
