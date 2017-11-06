module Test.E37 where

import Prelude

import Test.Testing(TestMe,assert,log)
import Data.List (List(..),(:),fromFoldable)
import Data.Foldable (all)
import Euler (digits)

import E37 (toInt,tabLeft,tabRight)

toList:: Int -> List Int
toList n = digits n # fromFoldable

testMe:: forall e. TestMe e
testMe = do
    log "Test E37"
    assert $ toInt (1:2:3:Nil) == 123
    assert $ toInt (3:5:1:4:Nil) == 3514

    assert $ all (toList >>> tabLeft) (37:47:373:Nil)
    assert $ all (toList >>> tabLeft >>> not) (19:149:163:Nil)

    assert $ all (toList >>> tabRight) (37:73:317:379:Nil)
    assert $ all (toList >>> tabRight >>> not) (47:61:127:181:Nil)