module Test.E46 where

import Prelude
import Test.Testing (TestMe,log,assert)
import Data.Maybe (isJust)
import Data.Foldable (all)
import E46 (conform)

testMe:: forall e. TestMe e
testMe = do
    log "E46"
    
    assert $ all (\n->conform n 1 # isJust) [9,15,21,25,27]