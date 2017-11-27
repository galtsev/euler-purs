module Test.E61 where

import Prelude
import Test.Testing (TestMe,log,assert)
import Data.Foldable (and)
import Data.Array as A
import Data.List (List(..),(:))
import Data.Maybe (Maybe(..))
import Data.Int (fromString)

import E61 (pair,first,chain,Nodes)

-- xx12:1212:1234:3412
-- xx23:2314:1455:5522
d1:: List Nodes
d1 = (1212:2314:1000:Nil):(1234:5522:2000:Nil):(3000:3412:1455:Nil):Nil

testMe:: forall e. TestMe e
testMe = do
    log "E61"
    
    assert $ and $ A.zipWith pair [1234, 2345, 9876, 5656, 1122, 4431] [3478, 4511, 7654, 5678, 2233, 3155]
    assert $ first fromString ("one":"12f":"67!":"76453":"123":"fasdf":Nil) == Just 76453

    assert $ chain 1299 9912 Nil d1 == Just (9912:1212:1234:3412:Nil)
    assert $ chain 2223 2323 Nil d1 == Just (2323:2314:1455:5522:Nil)