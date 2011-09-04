import Test.HUnit

import Data.Iter
import Data.Iter.String
import Data.Iter.MVar

import Control.Concurrent
import Control.Concurrent.MVar

testFilePath = "../test/testfile.txt"

tests =
    ["push and pull from mvar" ~: do
        var <- newEmptyMVar
        let i = ilines' . iterFile $ testFilePath
        forkIO $ pushToMVar var i

        let j = iterMVar var
        toList j >>= (@?= ["one", "two", "three"])
    ]

main = runTestTT $ test tests

