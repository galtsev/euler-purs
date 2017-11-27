module E61 where

import Prelude
import Data.Maybe (Maybe(..))
import Data.List (List(..),(:),filter)
import Euler (reverse,sum)

-- Triangle	 	P3,n=n(n+1)/2	 	1, 3, 6, 10, 15, ...
-- Square	 	P4,n=n*n	 	1, 4, 9, 16, 25, ...
-- Pentagonal	 	P5,n=n(3n−1)/2	 	1, 5, 12, 22, 35, ...
-- Hexagonal	 	P6,n=n(2n−1)	 	1, 6, 15, 28, 45, ...
-- Heptagonal	 	P7,n=n(5n−3)/2	 	1, 7, 18, 34, 55, ...
-- Octagonal	 	P8,n=n(3n−2)	 	1, 8, 21, 40, 65, ...

data Poly = Triangle | Square | Pentagonal | Hexagonal | Heptagonal | Octagonal

derive instance eqPoly:: Eq Poly

instance showPoly:: Show Poly where
    show Triangle = "Triangle"
    show Square  = "Square"
    show Pentagonal = "Pentagonal"
    show Hexagonal = "Hexagonal"
    show Heptagonal = "Heptagonal"
    show Octagonal = "Octagonal"

allPoly:: List Poly
allPoly = Triangle : Square : Pentagonal : Hexagonal : Heptagonal : Octagonal : Nil

poly:: Poly -> Int -> Int
poly Triangle n = n*(n+1)/2
poly Square n = n*n
poly Pentagonal n = n*(3*n-1)/2
poly Hexagonal n = n*(2*n-1)
poly Heptagonal n = n*(5*n-3)/2
poly Octagonal n = n*(3*n-2)

enlistPoly:: Poly -> Int -> Int -> List Int
enlistPoly p low high = ep Nil 1
    where
        ep acc i = case poly p i of
            pi | pi < low -> ep acc (i+1)
            pi | pi >= high -> acc
            pi -> ep (pi:acc) (i+1)

pair:: Int -> Int -> Boolean
pair a b = mod a 100 == b/100

type Nodes = List Int

first:: forall a b. (a -> Maybe b) -> List a -> Maybe b
first fn Nil = Nothing
first fn (x:xs) = case fn x of
    Just v -> Just v
    Nothing -> first fn xs

chain:: Int -> Int -> List Nodes -> List Nodes -> Maybe (List Int)
chain final current Nil Nil = if pair current final then Just (current:Nil) else Nothing
chain final current _ Nil = Nothing
chain final current checked (x:xs) = let
        fn v = chain final v Nil (reverse xs checked)
    in case first fn $ filter (pair current) x of
        Just ls -> Just (current:ls)
        Nothing -> chain final current (x:checked) xs

ee:: List Nodes -> Maybe (List Int)
ee Nil = Nothing
ee (x:xs) = first (\n -> chain n n Nil xs) x

e61:: Unit -> Maybe Int
e61 unit = map (\p->enlistPoly p 1000 10000) allPoly # ee # map sum