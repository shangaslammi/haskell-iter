
import Test.HUnit

import Data.Iter
import Data.Iter.String

import System.IO

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
        let i = iterFile "../test/testfile.txt"
        l <- toList i
        l @?= "one\ntwo\nthree"
        
    ,"test finalizer" ~: do
        fh <- openFile "../test/testfile.txt" ReadMode
        let i = itake 3 $ iterHandle fh
        l <- toList i
        l @?= "one"
        hIsClosed fh >>= (@? "handle closed succesfully after iteration")
    ]
        
main = runTestTT $ test tests

        