module Test.E62 where

import Prelude

import Test.Testing (TestMe,log,assert)
import E62

testMe:: forall e. TestMe e
testMe = do
    log "E62"
    
    assert true