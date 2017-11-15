module E46 where

import Prelude
import Data.Maybe (Maybe(..),isJust)

import Euler (prime)

conform:: Int -> Int -> Maybe Int
conform o _ | prime o = Just o
conform o n | 2*n*n>=o = Nothing
conform o n | prime (o-2*n*n) = Just o
conform o n = conform o (n+1)

ee:: Int -> Int
ee o | conform o 1 # isJust = ee (o+2)
ee o = o

e46:: Unit -> Int
e46 unit = ee 9