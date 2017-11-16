module Test.E47 where

import Prelude
import Test.Testing (TestMe,log,assert)
import Data.List (List(..),(:))
import E47 (factors)

testMe:: forall e. TestMe e
testMe = do
    log "E47"
    assert $ map factors [3,6,8,12,16*9, 98] == [3:Nil, 3:2:Nil, 8:Nil, 3:4:Nil, 9:16:Nil, 49:2:Nil]
