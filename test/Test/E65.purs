module Test.E65 where

import Prelude
import Test.Testing (TestMe,log,assert)
import Data.BigInt (fromInt)

import E65

testMe:: forall e. TestMe e
testMe = do
    log "E65"
    
    -- assert $ gcd1 (fromInt (3*5*19)) (fromInt (8*5*13)) == fromInt 5
    -- assert $ gcd1 (fromInt (8*9*73)) (fromInt (4*27*37)) == fromInt (4*9)
    -- assert $ gcd1 (fromInt (7*11*13)) (fromInt (5*17*19)) == fromInt 1