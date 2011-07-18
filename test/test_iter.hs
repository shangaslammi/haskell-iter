
import Test.HUnit
import Data.Iter

tests =
    ["to and from list" ~: do
        let l = [1,2,3,4,5]
        let i = iterList l
        l' <- toList i
        l' @?= l
    ]
        
main = runTestTT $ test tests

        