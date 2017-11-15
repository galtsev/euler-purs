module Test.E44 where

import Prelude
import Test.Testing (TestMe,log,assert)
import Data.Int (toNumber)
import Data.Foldable (all)
import E44 (pentagonal)

testMe:: forall e. TestMe e
testMe = do
    log "E44"

    assert $ all pentagonal (map toNumber [1, 5, 12, 22, 35, 51, 70, 92, 117, 145])
    assert $ all (pentagonal >>> not) (map toNumber [2,3,4,6,7,8,10,11,13,14,69,71,72])
