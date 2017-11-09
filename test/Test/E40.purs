module Test.E40 where

import Prelude
import Test.Testing (TestMe,log,assert)
import Data.String (charCodeAt)
import Data.Char (toCharCode)
import Data.Maybe (Maybe(..))
import Data.Array ((..))
import Data.Foldable (foldMap,all)

import E40 (dn)

z:: Int
z = toCharCode '0'

exp s n = charCodeAt (n-1) s # map (_-z)

testMe:: ∀ e. TestMe e
testMe = do
    log "E40"
    -- up to 900*3==2700
    let expected = (1..1000) # foldMap show
    let ex = exp expected
    
    assert $ dn 1 1 1 6 == Just 6
    assert $ dn 1 1 1 50 == ex 50
    assert $ all (\n -> dn 1 1 1 n == ex n) [5, 58, 587, 2654]
    