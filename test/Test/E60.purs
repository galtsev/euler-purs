module Test.E60 where

import Prelude
import Test.Testing (TestMe,log,assert,all)
import Data.List as L
import Data.List (List(..),(:))
import Data.Int (toNumber)
import Eu2 (prime)
import E60 (join,pair,mx)

triplet:: List Number -> Boolean
triplet (a:b:c:Nil) = pair a b && pair a c && pair b c
triplet _ = false

testMe:: forall e. TestMe e
testMe = do
    log "E60"
    
    assert $ map (join 123.0) [1.0, 2.0, 23.0, 543.0] == [1231.0, 1232.0, 12323.0, 123543.0]

    log "mx"
    
    assert let 
            lst = L.range 2 500 # map toNumber # L.filter prime
            tt = mx 3 lst 
        in L.length tt>0 &&  all triplet tt