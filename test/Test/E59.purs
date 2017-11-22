module Test.E59 where

import Prelude
import Test.Testing (TestMe,log,assert,all)
import Data.Array as A
import Data.String as S
import Data.Char as C

import E59 (correct, partition)

codes:: String -> Array Int
codes s = S.toCharArray s # map C.toCharCode

testMe:: forall e. TestMe e
testMe = do
    log "E59"
    
    assert $ "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ ,.;:!?-\n\"\'" # codes # all correct
    assert $ "(){}[]/\\|+*&$#@" # codes # all (correct >>> not)

    log "partition"
    assert $ partition (A.range 0 12) 4 == [[0,4,8,12], [1,5,9], [2,6,10], [3,7,11]]
