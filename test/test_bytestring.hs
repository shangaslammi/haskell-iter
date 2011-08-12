
import Test.HUnit

import Data.Iter
import Data.Iter.ByteString

import qualified Data.ByteString as B

import System.IO

testFilePath = "../test/testfile.bin"

tests =
    ["test iter binary file" ~: do
        let i = iterFile testFilePath
        b <- fmap B.unpack $ toByteString i
        b @?= [1..9]

    ,"test takeBytes" ~: do
        let i = bytes $ takeBytes 3 $ iterFile testFilePath
        toList i >>= (@?= [1,2,3])

    ,"test dropBytes" ~: do
        let i = bytes $ dropBytes 3 $ iterFile testFilePath
        toList i >>= (@?= [4..9])
    ]

main = runTestTT $ test tests
