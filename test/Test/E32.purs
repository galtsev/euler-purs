module Test.E32 where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.Assert (ASSERT, assert)

import Data.List (List(..),(:),foldMap)

import E32 (Permutation, fp, pan)

ff:: String -> Permutation -> String
ff s p = s <> " " <> (foldMap show p)

testMe :: forall e. Eff (assert :: ASSERT, console :: CONSOLE | e) Unit
testMe = do
    log "test foldPermutations"

    assert $ fp ff "hd" (1:Nil) == "hd 1"
    assert $ fp ff "hd" (1:2:Nil) == "hd 21 12"
    assert $ fp ff "hd" (1:2:3:Nil) == "hd 321 312 231 213 132 123"
    
    assert $ pan (3:9:1:8:6:7:2:5:4:Nil)
    assert $ not (pan (1:2:3:4:5:6:7:8:9:Nil))