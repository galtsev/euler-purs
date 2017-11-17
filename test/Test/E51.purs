module Test.E51 where

import Prelude
import Test.Testing (TestMe,log,assert)
import Data.List (List(..),(:))
import E51 (enumSimple, mutationsFor)

testMe:: forall e. TestMe e
testMe = do
    log "E51"
    assert $ map (enumSimple 0) [120, 1230, 101, 100, 120301, 1203004] == [1:Nil, 1:Nil, 10:Nil, 10:1:Nil, 1000:10:Nil, 10000:100:10:Nil]
    assert $ map (mutationsFor 0) [120, 3200, 1050] == [1:Nil, 11:10:1:Nil, 101:100:1:Nil]
