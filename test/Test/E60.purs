module Test.E60 where

import Prelude
import Test.Testing (TestMe,log,assert)

import E60 (join)

testMe:: forall e. TestMe e
testMe = do
    log "E60"
    
    assert $ map (join 123.0) [1.0, 2.0, 23.0, 543.0] == [1231.0, 1232.0, 12323.0, 123543.0]