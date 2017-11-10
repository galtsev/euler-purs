module Test.E43 where

import Prelude
import Test.Testing (TestMe,log,assert)

import E43 (correct)

testMe:: forall e. TestMe e
testMe = do
    log "E43"
    
    assert $ correct "1406357289"