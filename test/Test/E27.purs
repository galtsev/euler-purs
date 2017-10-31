module Test.E27 where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.Assert (ASSERT, assert)

import Data.BigInt (fromInt)

import E27 (primSeqLen, foldR)

testMe:: forall e. Eff (assert :: ASSERT, console :: CONSOLE | e) Unit
testMe = do
    log "start E27"

    assert $ primSeqLen 1 41 == fromInt 40
    assert $ primSeqLen (-79)  1601 == fromInt 80

    assert $ foldR (+) 3 3 7 == 3+3+4+5+6+7
    assert $ foldR (*) 11 2 5 == 11*2*3*4*5
    assert $ foldR (-) 30 6 8 == 30 - 6 - 7 - 8