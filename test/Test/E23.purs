module Test.E23 where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.Assert (ASSERT, assert)

import E23 (abundant)

testMe:: forall e. Eff (assert :: ASSERT, console :: CONSOLE | e) Unit
testMe = do
    log "start E23"
    assert $ abundant 12
    -- 3*4*5=60 | 1+ 2+ 3+ 4+ 5+ 6+ 10+ 12+ 15+ 20+ 30 = 108
    assert $ abundant 60
    -- 3*5=15 | 1+3+5 = 9
    assert $ not (abundant 15)
    -- 5*8 = 40 | 1+2+4+5+10+20=42
    assert $ abundant 40
    -- 5*7 = 35 | 1+5+7 = 13
    assert $ not (abundant 35)