import Test.HUnit

import Data.Iter
import Data.Iter.State

testFilePath = "../test/testfile.txt"

tests =
    ["test getNext" ~: do
        let i = iterList [1,2,3]
        let s = do
            a <- getNext
            b <- getNext
            c <- getNext
            return (a,b,c)
        runIterIO s i >>= (@?= (1,2,3))
    ]

main = runTestTT $ test tests
