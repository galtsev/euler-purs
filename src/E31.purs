module E31 where

import Prelude
import Data.List (List(..),(:))

coins:: List Int
coins = 200:100:50:20:10:5:2:1:Nil

cnt:: Int -> List Int -> Int
cnt 0 _ = 1
cnt n (1:Nil) = 1
cnt n Nil = 0
cnt n l@(x:xs) | n>=x = cnt (n-x) l + cnt n xs
cnt n (x:xs) = cnt n xs

e31:: Unit -> Int
e31 unit = cnt 200 coins