module Test.Eu2 where

import Prelude
import Test.Testing (TestMe,log,assert,all)
import Data.Int (toNumber)
import Eu2 (prime)

testMe:: forall e. TestMe e
testMe = do
    log "Eu2"
    
    assert $ all prime $ map toNumber [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47]
    assert $ all (prime>>>not) $ map toNumber [1,4,6,8,9,10,12,14,15,16,18,20,21,22,24,121]