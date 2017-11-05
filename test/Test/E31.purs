module Test.E31 where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.Assert (ASSERT, assert)

import Data.List (List(..),(:))

import E31 (cnt)

testMe:: forall e. Eff (assert :: ASSERT, console :: CONSOLE | e) Unit
testMe = do
    log "start E31"

    assert $ cnt 37 (1:Nil) == 1
    assert $ cnt 37 (2:Nil) == 0
    -- 0..18 of 2p, remain by 1p
    assert $ cnt 37 (2:1:Nil) == 19
    assert $ cnt 37 (10:1:Nil) == 4
    -- 20, 20+10, 10, 2*10, 3*10, 37*1
    assert $ cnt 37 (100:50:20:10:1:Nil) == 6
    -- 2*20, 20, 20+2*10, 20+10, 4*10, 3*10, 2*10, 10, all 1
    assert $ cnt 40 (20:10:1:Nil) == 9
    -- 2*20, 20+2*10, 4*10
    assert $ cnt 40 (20:10:Nil) == 3
