{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit

import Data.Iter
import Data.Iter.Text

import qualified Data.Text as T

testFilePath = "../test/testfile.txt"

tests =
    ["test isplitOn" ~: do
        let i = isplitOn "," $ return $ T.pack "one,two,three"
        l <- isequence $ imap toText i
        l @?= ["one", "two", "three"]
    ]

main = runTestTT $ test tests
