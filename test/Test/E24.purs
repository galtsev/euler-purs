module Test.E24 where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.Assert (ASSERT, assert)

import Data.List ((:),List(..),(..),reverse)
import E24 (next,n2,ee)

testMe :: forall e. Eff (assert :: ASSERT, console :: CONSOLE | e) Unit
testMe = do
    log "test e24"
    assert $ n2 1 (2:Nil) == 2:1:Nil
    assert $ n2 1 (3:2:Nil) == 2:1:3:Nil
    assert $ n2 2 (3:1:Nil) == 3:1:2:Nil

    assert $ next (3:2:1:Nil) == 2:3:1:Nil
    assert $ next (2:3:1:Nil) == 3:1:2:Nil
    assert $ next (2:3:4:1:Nil) == 4:3:1:2:Nil

    -- 1234 -> 1243 -> 1324
    assert $ ee  3 (4:3:2:1:Nil) == 4:2:3:1:Nil
    -- 4!=24 012345 6789 .. 01234 65 789
    assert $ ee 25 (reverse (0..9)) == 9:8:7:5:6:4:3:2:1:0:Nil

