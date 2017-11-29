module E65 where

import Prelude
import Data.BigInt (BigInt)
import Data.BigInt as B
import Data.String as S
import Data.List (List(..),(..),(:),take,foldMap,foldl,reverse)
import Data.Array as A
import Data.Ratio (Ratio,(%),numerator)
import Data.Int (fromString)
import Data.Foldable (sum)

type Rational = Ratio BigInt

firstX:: Int -> List Int
firstX n = 2:1:foldMap (\i->i*2:1:1:Nil) (1..(n/3+1)) # take n

fromInt:: Int -> Rational
fromInt i = B.fromInt i % one

go:: List Int -> Rational
go (x:xs) = foldl (\acc a->fromInt a+one/acc) (fromInt x) xs
go Nil = fromInt 0

ee:: Int -> Rational
ee n = go (firstX n # reverse)

e65:: Unit -> Int
e65 unit = ee 100 # numerator # B.toString >>> S.toCharArray # A.mapMaybe (S.singleton>>>fromString) # sum