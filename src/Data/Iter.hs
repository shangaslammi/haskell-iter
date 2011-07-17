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

infixr 6 :::

instance Functor Iter where
    fmap _ StopIteration = StopIteration
    fmap f (a ::: i)     = f a ::: fmap f i
    fmap f (IterIO io)   = IterIO $ fmap (fmap f) io

instance Applicative Iter where
    pure a  = a ::: StopIteration

    StopIteration <*> _ = StopIteration
    _ <*> StopIteration = StopIteration
    (f ::: i) <*> j     = fmap f j +++ (i <*> j)
    (IterIO io) <*> j   = IterIO $ fmap (<*> j) io

instance Monad Iter where
    return a = a ::: StopIteration

    StopIteration >>= _ = StopIteration
    (a ::: i)     >>= f = f a +++ (i >>= f)
    (IterIO io)   >>= f = IterIO $ fmap (>>= f) io

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

next :: Iter a -> Iter (a, Iter a)
next StopIteration = StopIteration
next (a ::: i)     = return (a, i)
next (IterIO io)   = IterIO $ fmap next io

(+++) :: Iter a -> Iter a -> Iter a
StopIteration +++ j = j
i +++ StopIteration = i
(a ::: i) +++ j     = a ::: (i +++ j)
(IterIO io) +++ j   = IterIO $ fmap (+++j) io

infixr 5 +++

ihead :: Iter a -> IO a
ihead = fmap (fst.fromJust) . nextIO

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

itake :: Int -> Iter a -> Iter a
itake 0 _ = StopIteration
itake n i = do
    (a, i') <- next i
    a ::: itake (n-1) i'

idrop :: Int -> Iter a -> Iter a
idrop 0 i = i
idrop n i = do
    (_, i') <- next i
    idrop (n-1) i'

ireverse :: Iter a -> Iter a
ireverse = join . ifoldl (flip (:::)) StopIteration

iintersperse :: a -> Iter a -> Iter a
iintersperse x i = do
    (a, i') <- next i
    a ::: x ::: iintersperse x i'

ifoldr :: (a -> b -> b) -> b -> Iter a -> Iter b
ifoldr _ acc StopIteration = return acc
ifoldr f acc (a ::: i)     = fmap (f a) $ ifoldr f acc i
ifoldr f acc (IterIO io)   = IterIO $ fmap (ifoldr f acc) io

ifoldl :: (b -> a -> b) -> b -> Iter a -> Iter b
ifoldl _ acc StopIteration = return acc
ifoldl f acc (a ::: i)     = acc `seq` ifoldl f (f acc a) i
ifoldl f acc (IterIO io)   = IterIO $ fmap (ifoldl f acc) io

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

iand :: Iter Bool -> IO Bool
iand = ifoldlIO (&&) True

ior :: Iter Bool -> IO Bool
ior = ifoldlIO (||) False

iany :: (a -> Bool) -> Iter a -> IO Bool
iany f = ior . imap f

iall :: (a -> Bool) -> Iter a -> IO Bool
iall f = iand . imap f

isum :: Num a => Iter a -> IO a
isum = ifoldlIO (+) 0

iproduct :: Num a => Iter a -> IO a
iproduct = ifoldlIO (*) 1

imaximum :: Ord a => Iter a -> IO a
imaximum i = ihead $ do
    (a, i') <- next i
    ifoldl max a i'

iminimum :: Ord a => Iter a -> IO a
iminimum i = ihead $ do
    (a, i') <- next i
    ifoldl min a i'

----- Evaluate fold results in the IO Monad -----
ifoldrIO :: (a -> b -> b) -> b -> Iter a -> IO b
ifoldrIO f acc i = liftM (fst.fromJust) $ nextIO (ifoldr f acc i)

ifoldlIO :: (b -> a -> b) -> b -> Iter a -> IO b
ifoldlIO f acc i = liftM (fst.fromJust) $ nextIO (ifoldl f acc i)

----- Conversion from and to lists -----
iterList :: [a] -> Iter a
iterList = foldr (:::) StopIteration

toList :: Iter a -> IO [a]
toList StopIteration = return []
toList (a ::: i)     = fmap (a:) $ toList i

----- Sequence IO actions from iterators -----
ifor :: Iter a -> (a -> IO b) -> IO [b]
ifor i f = isequence $ fmap f i

isequence :: Iter (IO a) -> IO [a]
isequence StopIteration = return []
isequence (a ::: i)     = do
    x  <- a
    xs <- isequence i
    return (x:xs)
isequence (IterIO io)   = io >>= isequence

isequence_ :: Iter (IO a) -> IO ()
isequence_ = join . ifoldrIO (>>) (return ())
