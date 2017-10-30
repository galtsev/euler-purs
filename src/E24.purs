module E24 where

import Prelude
import Data.List (sort,filter,(:),List(..),(..),reverse,foldMap)
import Data.Traversable (minimum)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

target:: Int
target = 1000*1000

next:: List Int -> List Int
next l = n1 l Nil

-- step 1 - take off growing tail
n1:: List Int -> List Int -> List Int
n1 (x:xs) Nil = n1 xs (x:Nil)
n1 (x:xs) b@(y:ys) = if x>y then n1 xs (x:b) else  reverse (n2 x b) <> xs
n1 a b = unsafeCrashWith $ "wrong args in n1 " <> show a <> " - " <> show b

n2:: Int -> List Int -> List Int
n2 x xs = let
        newHead = unsafePartial (fromJust $ filter (_>x) xs # minimum)
        newTail = x : filter (_/=newHead) xs
    in newHead : sort newTail

e24:: Unit -> String
e24 unit = ee target (reverse (0..9)) # reverse # foldMap show

ee:: Int -> List Int -> List Int
ee 1 l = l
ee n l = ee (n-1) (next l)