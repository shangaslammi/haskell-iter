import Test.HUnit

import Data.Iter
import Data.Iter.State

testFilePath = "../test/testfile.txt"

tests =
    ["test runIStateIO" ~: do
        let i = iterList [1,2,3]
        let s = do
            a <- getNext
            b <- getNext
            return (a,b)
        (a,i') <- runIStateIO s i
        a @?= (1,2)
        toList i' >>= (@?= [3])

    ,"test getNext" ~: do
        let i = iterList [1,2,3]
        let s = do
            a <- getNext
            b <- getNext
            c <- getNext
            return (a,b,c)
        runIterIO s i >>= (@?= (1,2,3))

    ,"test withNext" ~: do
        let i = iterList [1..10]
        let s = withNext (+2)
        runIterIO s i >>= (@?= 3)

    ,"test withNext2" ~: do
        let i = iterList [1..10]
        let s = withNext2 (+)
        (a,i') <- runIStateIO s i
        a @?= 3
        toList i' >>= (@?= [3..10])

    ,"test tryNext" ~: do
        let i = iterList [1,2,3]
        let s = do
            a <- tryNext
            b <- tryNext
            c <- tryNext
            d <- tryNext
            return (a,b,c,d)

        runIterIO s i >>= (@?= (Just 1, Just 2, Just 3, Nothing))
    
    ,"test peekNext" ~: do
        let i = iterList [1,2]
        let s = do
            a <- peekNext
            b <- tryNext
            c <- peekNext
            d <- tryNext
            e <- peekNext
            return (a,b,c,d,e)
            
        runIterIO s i >>= (@?= (Just 1, Just 1, Just 2, Just 2, Nothing))
    ]

main = runTestTT $ test tests
