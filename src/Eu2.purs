module Eu2 where

import Prelude
import Math ((%))

prime:: Number -> Boolean
prime n | n<2.0 = false
prime n = pp 2.0
    where
        pp i | i*i > n = true
        pp i | n % i == 0.0 = false
        pp i = pp (i+1.0)