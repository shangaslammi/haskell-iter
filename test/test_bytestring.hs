
import Test.HUnit

import Data.Iter
import Data.Iter.ByteString

import qualified Data.ByteString as B

import System.IO

testFilePath = "../test/testfile.bin"

tests =
    ["iter binary file" ~: do
        let i = iterFile testFilePath
        l <- fmap (B.unpack . B.concat) $ toList i
        l @?= [1..9]
    ]

main = runTestTT $ test tests
