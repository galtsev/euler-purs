module Test.Euler where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.Assert (ASSERT, assert)

import Data.List as L
import Data.List (List(..),(:))
import Data.Traversable (all)
import Data.String as S

import Euler (prime,foldRange, foldPermutations)

testMe:: forall e. Eff (assert :: ASSERT, console :: CONSOLE | e) Unit
testMe = do
    log "Testing Euler module"
    
    assert $ all prime [2,3,5,7,11,13,19,37,73]
    assert $ all (prime >>> not) [4,6,8,9,10,12,14,15,16,18,20]

    assert $ foldRange (+) 0 1 10 == 55
    assert $ foldRange (*) 1 3 5 == 3*4*5
    assert $ foldRange (\a i->a+i*i) 0 2 6 == 4+9+16+25+36

    let p3 = foldPermutations (\acc p-> L.foldMap show p:acc) Nil (1:2:3:Nil)
    assert $ all (\x -> S.length x==3) p3
    assert $ all (\x-> L.elem x p3) ("123":"132":"213":"231":"312":"321":Nil)
    assert $ L.length p3 == 6