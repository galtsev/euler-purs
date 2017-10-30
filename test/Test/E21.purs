module Test.E21 where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.Assert (ASSERT, assert)

import E21 (divs, amicable)

testMe :: forall e. Eff (assert :: ASSERT, console :: CONSOLE | e) Unit
testMe = do
    log "test divs"
    assert $ divs 7 == 1
    assert $ divs 6 == (1+2+3)
    assert $ divs (7*11) == (1+7+11) -- 1+7+11 == 19
    assert $ divs (1+7+11) == 1
    assert $ divs (7*7) == (1+7)
    assert $ divs 220 == 284
    assert $ divs 284 == 220

    assert $ amicable 220
    assert $ amicable 284
    assert $ not (amicable 6)
    assert $ not (amicable 7)
    assert $ not (amicable 19)
