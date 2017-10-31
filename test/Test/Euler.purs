module Test.Euler where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.Assert (ASSERT, assert)

import Data.Traversable (all)

import Euler (prime,foldRange)

testEuler:: forall e. Eff (assert :: ASSERT, console :: CONSOLE | e) Unit
testEuler = do
    log "Testing Euler module"
    
    assert $ all prime [2,3,5,7,11,13,19,37,73]
    assert $ all (prime >>> not) [4,6,8,9,10,12,14,15,16,18,20]

    assert $ foldRange (+) 0 11 == 55
    assert $ foldRange (\a i->a*(i+1)) 1 5 == 1*1*2*3*4*5
    assert $ foldRange (\a i->a+i*i) 0 6 == 1+4+9+16+25
