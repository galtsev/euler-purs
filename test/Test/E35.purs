module Test.E35 where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.Assert (ASSERT, assert)

import Data.List (List(..),(:),length,intersect)

import E35 (rots)

check:: Int -> Int -> List Int -> Boolean
check v n expect = let rs = rots v in length rs == n && intersect expect rs == expect

testMe :: forall e. Eff (assert :: ASSERT, console :: CONSOLE | e) Unit
testMe = do
    log "test E35"
    assert $ check 123 3 (123:312:231:Nil)
    assert $ check 7654 4 (7654:4765:5476:6547:Nil)

