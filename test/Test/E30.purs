module Test.E30 where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.Assert (ASSERT, assert)

import E30 (sd)

p5:: Int -> Int
p5 n = n*n*n*n*n

testMe:: forall e. Eff (assert :: ASSERT, console :: CONSOLE | e) Unit
testMe = do
    log "start E30"
    
    assert $ sd 12 == 1 + 32
    log "12 passed"
    assert $ sd 34 == p5 3 + p5 4
    log "34 passed"
    assert $ sd 999999 == 6*(p5 9)