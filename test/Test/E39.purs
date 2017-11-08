module Test.E39 where

import Prelude
import Test.Testing (TestMe,log,assert)
import Data.Map (Map,empty,insert,lookup)
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..))

import E39 (add,getMax)

mkMap:: Unit -> Map Int Int
mkMap unit = empty # insert 1 1 # insert 2 4 # insert 3 9 # insert 4 1

testMe:: forall e. TestMe e
testMe = do
    log "E39"
    
    assert $ let c = foldr add empty [1,1,2,4] in (lookup 1 c==Just 2) && (lookup 2 c==Just 1) && (lookup 3 c==Nothing)
    assert $ let r = getMax (mkMap unit) in r.p==3 && r.m==9