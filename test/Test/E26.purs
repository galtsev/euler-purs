module Test.E26 where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.Assert (ASSERT, assert)

import Data.Maybe (Maybe(..))
import Data.List (List(..),(:))

import E26 (period,findIndex,maxPeriod)

testMe :: forall e. Eff (assert :: ASSERT, console :: CONSOLE | e) Unit
testMe = do
    log "test divs"
    
    assert $ findIndex 1 (1:Nil) == Just 1
    assert $ findIndex 2 (1:2:Nil) == Just 2
    assert $ findIndex 3 (1:2:3:1:2:3:Nil) == Just 3
    assert $ findIndex 4 (1:2:3:Nil) == Nothing

    assert $ period 3 == 1
    assert $ period 7 == 6
    assert $ period 2 == 0
    assert $ period 10 == 0
    
    assert $ maxPeriod 0 0 5 == 3
    assert $ maxPeriod 0 0 6 == 6
    assert $ maxPeriod 0 0 7 == 7
    assert $ maxPeriod 0 0 10 == 7