
import Test.HUnit

import Data.Iter
import Data.Iter.ByteString

import qualified Data.ByteString as B

import System.IO

testFilePath = "../test/testfile.bin"

tests =
    ["iter binary file" ~: do
        let i = iterFile testFilePath
        b <- fmap B.unpack $ toByteString i
        b @?= [1..9]
    ]

main = runTestTT $ test tests
