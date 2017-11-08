module Test.E38 where

import Prelude
import Test.Testing (TestMe,assert,log)

import Data.Maybe (isJust,isNothing)
import Data.String (Pattern(..),Replacement(..),replaceAll)

import E38 (pdd,pandig)

trim:: String -> String
trim s = replaceAll (Pattern " ") (Replacement "") s

testMe:: forall e. TestMe e
testMe = do
    log "E38"
    
    assert $ isJust (pdd "123456789")
    assert $ isJust (pdd "123459876")
    assert $ isJust (pdd "987123456")
    assert $ isNothing (pdd "123456782")
    assert $ isNothing (pdd "1234567891")

    assert $ pandig 10 == trim "10 20 30 40 50"
    assert $ pandig 21 == trim "21 42 63 84 105"
    assert $ pandig 101 == trim "101 202 303"