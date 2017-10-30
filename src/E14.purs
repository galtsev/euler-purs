module E14 where

import Prelude
import Data.BigInt (BigInt,fromInt,even)
import Data.Monoid (class Monoid)
import Data.Array (foldMap,(..),foldl)

n1:: BigInt
n1 = fromInt 1

n2:: BigInt
n2 = fromInt 2

n3:: BigInt
n3 = fromInt 3

target:: Int
target = 1000000-1

cLen:: Int -> Int
cLen x = cl (fromInt x) 1
	where
		cl v n | v==n1 = n
		cl v n | even v = cl (v/n2) (n+1)
		cl v n = cl (v*n3+n1) (n+1)

-- | start point, sequence length
data Pair = Pair Int Int

instance semigroupPair:: Semigroup Pair where
	append p1@(Pair v1 l1) (Pair v2 l2) | l1>l2 = p1
	append _ p2 = p2

instance monoidPair:: Monoid Pair where
	mempty = Pair 0 0

e14:: Unit -> Int
e14 unit = foldMap (\i->Pair i (cLen i)) (1..target) # \(Pair i _)-> i

type P = {v::Int,l::Int}

ffun:: P->P->P
ffun a b | a.l>b.l = a
ffun _ b = b

e14':: Unit -> Int
e14' unit = (1..target) # map (\i->{v:i,l:cLen i}) # foldl ffun {v:0,l:0} # \r->r.v