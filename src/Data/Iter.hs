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
    | Finalize (IO ()) (Iter a)

infixr 6 :::

instance Functor Iter where
    fmap _ StopIteration  = StopIteration
    fmap f (a ::: i)      = f a ::: fmap f i
    fmap f (IterIO io)    = IterIO $ fmap (fmap f) io
    fmap f (Finalize z i) = Finalize z (fmap f i)

instance Applicative Iter where
    pure a  = a ::: StopIteration

    StopIteration <*> _  = StopIteration
    _ <*> StopIteration  = StopIteration
    (f ::: i) <*> j      = fmap f j +++ (i <*> j)
    (IterIO io) <*> j    = IterIO $ fmap (<*> j) io
    (Finalize z i) <*> j = Finalize z (i <*> j)

instance Monad Iter where
    return a = a ::: StopIteration

    StopIteration  >>= _ = StopIteration
    (a ::: i)      >>= f = f a +++ (i >>= f)
    (IterIO io)    >>= f = IterIO $ fmap (>>= f) io
    (Finalize z i) >>= f = Finalize z (i >>= f)

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
nextIO (Finalize f i) = do
    n <- nextIO i
    case n of
        Nothing      -> f >> return Nothing
        Just (a, i') -> returnIO (a, Finalize f i')

next :: Iter a -> Iter (a, Iter a)
next StopIteration  = StopIteration
next (a ::: i)      = return (a, i)
next (IterIO io)    = IterIO $ fmap next io
next (Finalize f i) = IterIO $ do
    n <- nextIO i
    case n of
        Nothing     -> f >> return StopIteration
        Just (a,i') -> return $ return (a, Finalize f i')

peek :: Iter a -> Iter (Maybe (a, Iter a))
peek i = liftIO $ nextIO i

(!::) :: a -> Iter a -> Iter a
a !:: (Finalize f i) = Finalize f (a ::: i)
a !:: i              = a ::: i

infixr 6 !::

(+++) :: Iter a -> Iter a -> Iter a
i +++ (Finalize f j) = Finalize f (i +++ j)
StopIteration +++ j  = j
i +++ StopIteration  = i
(a ::: i) +++ j      = a ::: (i +++ j)
(IterIO io) +++ j    = IterIO $ fmap (+++j) io

infixr 5 +++

ihead :: Iter a -> IO a
ihead = fmap (fst.fromJust) . nextIO

listify :: Iter a -> Iter [a]
listify = liftIO . toList

ifEmpty :: Iter b -> (Iter a -> Iter b) -> Iter a -> Iter b
ifEmpty then_ else_ i = do
    p <- peek i
    case p of
        Nothing -> then_
        Just (a,i') -> else_ (a !:: i')

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
ifilter f (Finalize z i) = Finalize z (ifilter f i)
ifilter f i = do
    x <- i
    guard (f x)
    return x

ireverse :: Iter a -> Iter a
ireverse = join . ifoldl (flip (:::)) StopIteration

iintersperse :: a -> Iter a -> Iter a
iintersperse x (Finalize z i) = Finalize z (iintersperse x i)
iintersperse x StopIteration  = StopIteration
iintersperse x (IterIO io)    = IterIO $ fmap (iintersperse x) io
iintersperse x (a ::: StopIteration) = return a
iintersperse x (a ::: (IterIO io))   = IterIO $ do
    i <- io
    return $ iintersperse x (a !:: i)
iintersperse x (a ::: i)      = a ::: x ::: iintersperse x i

ifoldr :: (a -> b -> b) -> b -> Iter a -> Iter b
ifoldr _ acc StopIteration  = return acc
ifoldr f acc (a ::: i)      = fmap (f a) $ ifoldr f acc i
ifoldr f acc (IterIO io)    = IterIO $ fmap (ifoldr f acc) io
ifoldr f acc (Finalize z i) = Finalize z (ifoldr f acc i)

ifoldl :: (b -> a -> b) -> b -> Iter a -> Iter b
ifoldl _ acc StopIteration  = return acc
ifoldl f acc (a ::: i)      = acc `seq` ifoldl f (f acc a) i
ifoldl f acc (IterIO io)    = IterIO $ fmap (ifoldl f acc) io
ifoldl f acc (Finalize z i) = Finalize z (ifoldl f acc i)

izip :: Iter a -> Iter b -> Iter (a,b)
izip (Finalize z i) (Finalize x j) = Finalize (z >> x) (izip i j)
izip (Finalize z i) j    = Finalize z (izip i j)
izip i (Finalize z j)    = Finalize z (izip i j)
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
iunfold f (Finalize z i) = Finalize z (iunfold f i)
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


----- Take and drop -----
itake :: Int -> Iter a -> Iter a
itake n (Finalize z i) = Finalize z (itake n i)
itake 0 _ = StopIteration
itake n i = do
    (a, i') <- next i
    a ::: itake (n-1) i'

idrop :: Int -> Iter a -> Iter a
idrop n (Finalize z i) = Finalize z (idrop n i)
idrop 0 i = i
idrop n i = do
    (_, i') <- next i
    idrop (n-1) i'

itakeUntil :: (a -> Bool) -> Iter a -> Iter a
itakeUntil f (Finalize z i) = Finalize z (itakeUntil f i)
itakeUntil f i = do
    (a,i') <- next i
    when (f a) StopIteration
    a !:: itakeUntil f i'

itakeWhile :: (a -> Bool) -> Iter a -> Iter a
itakeWhile f i = itakeUntil (not.f) i

idropUntil :: (a -> Bool) -> Iter a -> Iter a
idropUntil f (Finalize z i) = Finalize z (idropUntil f i)
idropUntil f i = do
    (a, i') <- next i
    if (f a)
        then a !:: i'
        else idropUntil f i'

idropWhile :: (a -> Bool) -> Iter a -> Iter a
idropWhile f i = idropUntil (not.f) i

----- Splitting iterators into sub-iterators -----
isplitAt :: Int -> Iter a -> Iter (Iter a, Iter a)
isplitAt n (Finalize z i) = do
    (j,k) <- isplitAt n i
    return (j, Finalize z k)
isplitAt 0 i = return (StopIteration, i)
isplitAt n i = do
    p <- peek i
    case p of
        Nothing -> return (StopIteration, StopIteration)
        Just (a, i') -> do
            (j,k) <- isplitAt (n-1) i'
            return (a !:: j, k)

ispan :: (a -> Bool) -> Iter a -> Iter (Iter a, Iter a)
ispan f (Finalize z i) = do
    (j,k) <- ispan f i
    return (j, Finalize z k)
ispan f i = do
    p <- peek i
    case p of
        Nothing -> return (StopIteration, StopIteration)
        Just (a,i') -> if f a
            then do
                (j,k) <- ispan f i'
                return (a !:: j, k)
            else return (StopIteration, a !:: i')

ibreak :: (a -> Bool) -> Iter a -> Iter (Iter a, Iter a)
ibreak f i = ispan (not.f) i

isplitWhen :: (a -> Bool) -> Iter a -> Iter (Iter a)
isplitWhen f = iunfold step where
    step i = do
        p <- peek i
        case p of
            Nothing -> StopIteration
            Just (a,i') -> do
                (j,k) <- ibreak f (a !:: i')
                return (j, idrop 1 k)

isplitWhen' :: (a -> Bool) -> Iter a -> Iter [a]
isplitWhen' f = iconcatMap listify . isplitWhen f

----- Evaluate fold results in the IO Monad -----
ifoldrIO :: (a -> b -> b) -> b -> Iter a -> IO b
ifoldrIO f acc i = liftM (fst.fromJust) $ nextIO (ifoldr f acc i)

ifoldlIO :: (b -> a -> b) -> b -> Iter a -> IO b
ifoldlIO f acc i = liftM (fst.fromJust) $ nextIO (ifoldl f acc i)

----- Conversion from and to lists -----
iterList :: [a] -> Iter a
iterList = foldr (:::) StopIteration

toList :: Iter a -> IO [a]
toList StopIteration  = return []
toList (a ::: i)      = fmap (a:) $ toList i
toList (IterIO io)    = io >>= toList
toList (Finalize z i) = do
    l <- toList i
    z
    return l

----- Sequence IO actions from iterators -----
ifor :: Iter a -> (a -> IO b) -> IO [b]
ifor i f = isequence $ fmap f i

isequence :: Iter (IO a) -> IO [a]
isequence StopIteration  = return []
isequence (a ::: i)      = do
    x  <- a
    xs <- isequence i
    return (x:xs)
isequence (IterIO io)    = io >>= isequence
isequence (Finalize z i) = do
    l <- isequence i
    z
    return l

isequence_ :: Iter (IO a) -> IO ()
isequence_ = join . ifoldrIO (>>) (return ())
