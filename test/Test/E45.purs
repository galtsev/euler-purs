module Test.E45 where

import Prelude
import Test.Testing (TestMe,log,assert)
import Data.Int (toNumber)
import Data.Array ((..))
import Data.Foldable (all)

import E45 (penta,hexa)

pp:: Number -> Number
pp n = n*(3.0*n-1.0)/2.0
hh n = n*(2.0*n-1.0)

testMe:: forall e. TestMe e
testMe = do
    log "E45"
    assert $ all penta (map (toNumber >>> pp) (1..4000))
    assert $ all hexa (map (toNumber >>> hh) (1..4000))
