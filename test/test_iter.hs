
import Test.HUnit

import Data.Iter
import Data.Iter.String

import System.IO

testFilePath = "../test/testfile.txt"

tests =
    ["to and from list" ~: do
        let l = [1..5]
        let i = iterList l
        l' <- toList i
        l' @?= l

    ,"test imap" ~: do
        let l = [1..5]
        let i = imap (*2) $ iterList l
        l' <- toList i
        l' @?= map (*2) l

    ,"test ifilter" ~: do
        let l = [1..10]
        let i = ifilter odd $ iterList l
        l' <- toList i
        l' @?= filter odd l

    ,"test +++" ~: do
        let i = iterList [1..3]
        let j = iterList [4..6]
        let k = iterList [7..10]

        let i' = i +++ j +++ k
        l <- toList i'
        l @?= [1..10]

    ,"test iconcat" ~: do
        let i = iterList $ map iterList ["foo", "bar", "asdf"]
        l <- toList $ iconcat i
        l @?= "foobarasdf"

    ,"test iconcatMap" ~: do
        let i = iconcatMap iterList $ iterList ["foo", "bar", "asdf"]
        l <- toList i
        l @?= "foobarasdf"

    ,"test itake and idrop" ~: do
        let i = itake 10 $ idrop 10 $ iterList [1..]
        l <- toList i
        l @?= [11..20]

    ,"test ireverse" ~: do
        let i = ireverse $ itake 10 $ iterList [1..]
        l <- toList i
        l @?= reverse [1..10]

    ,"test iintersperse0" ~: do
        let i = iintersperse '.' $ iterList ""
        l <- toList i
        l @?= ""

    ,"test iintersperse1" ~: do
        let i = iintersperse '.' $ iterList "a"
        l <- toList i
        l @?= "a"

    ,"test iintersperse3" ~: do
        let i = iintersperse '.' $ iterList "abc"
        l <- toList i
        l @?= "a.b.c"

    ,"test iterFile" ~: do
        let i = iterFile testFilePath
        l <- toList i
        l @?= "one\ntwo\nthree"

    ,"test finalizer" ~: do
        fh <- openFile testFilePath ReadMode
        let i = itake 3 $ iterHandle fh
        l <- toList i
        l @?= "one"
        hIsClosed fh >>= (@? "handle closed succesfully after iteration")

    ,"test isplitAt" ~: do
        let i = isplitAt 3 $ iterList [1..10]
        (j,k) <- ihead i
        toList j >>= (@?= [1..3])
        toList k >>= (@?= [4..10])

    ,"test isplitAt handle with out-of-order iteration" ~: do
        fh <- openFile testFilePath ReadMode
        let i = isplitAt 3 $ iterHandle fh
        (j,k) <- ihead i
        let k' = itake 3 $ idrop 1 k
        toList k' >>= (@?= "two")
        toList j  >>= (@?= "one")
        hIsClosed fh >>= (@? "handle closed succesfully after iteration")

    ,"test itakeWhile" ~: do
        let i = itakeWhile (<5) $ iterList [1..]
        toList i >>= (@?= [1..4])

    ,"test itakeUntil" ~: do
        let i = itakeUntil (>5) $ iterList [1..]
        toList i >>= (@?= [1..5])

    ,"test idropWhile" ~: do
        let i = idropWhile (<5) $ iterList [1..10]
        toList i >>= (@?= [5..10])

    ,"test idropUntil" ~: do
        let i = idropUntil (>5) $ iterList [1..10]
        toList i >>= (@?= [6..10])

    ,"test ispan" ~: do
        let i = do
            (j,k) <- ispan (<5) $ iterList [1..]
            return (j, itake 5 k)

        (j,k) <- ihead i
        toList j >>= (@?= [1..4])
        toList k >>= (@?= [5..9])

    ,"test ibreak" ~: do
        let i = do
            (j,k) <- ibreak (==5) $ iterList [1..]
            return (j, itake 5 k)
        (j,k) <- ihead i
        toList j >>= (@?= [1..4])
        toList k >>= (@?= [5..9])

    ,"test isplitWhen" ~: do
        let i = isplitWhen (==3) $ iterList [1,2,3,4,5,6,3,1]
        Just (a, j) <- nextIO i
        toList a >>= (@?= [1,2])
        Just (b, k) <- nextIO j
        toList b >>= (@?= [4,5,6])
        Just (c, l) <- nextIO k
        toList c >>= (@?= [1])
        l' <- toList l
        null l' @? "end of iteration"

    ,"test isplitWhen'" ~: do
        let i = isplitWhen' (==3) $ iterList [1,2,3,4,5,6,3,1]
        toList i >>= (@?= [[1,2],[4,5,6],[1]])

    ,"test iwords'" ~: do
        let i = iwords' $ iterList "one two   three  "
        toList i >>= (@?= ["one", "two", "three"])
    ]

main = runTestTT $ test tests
