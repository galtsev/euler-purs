module E17 where

import Prelude
import Partial.Unsafe (unsafeCrashWith)
import Data.String (replaceAll,Pattern(..),Replacement(..),length)
import Data.Traversable (sum)
import Data.List ((..))

w1:: Int -> String
w1 1 = "one"
w1 2 = "two"
w1 3 = "three"
w1 4 = "four"
w1 5 = "five"
w1 6 = "six"
w1 7 = "seven"
w1 8 = "eight"
w1 9 = "nine"
w1 10 = "ten"
w1 11 = "eleven"
w1 12 = "twelve"
w1 13 = "thirteen"
w1 14 = "fourteen"
w1 15 = "fifteen"
w1 16 = "sixteen"
w1 17 = "seventeen"
w1 18 = "eighteen"
w1 19 = "nineteen"
w1 n = unsafeCrashWith ("unhandled number in w1: " <> show n)

dec:: Int -> String
dec 2 = "twenty"
dec 3 = "thirty"
dec 4 = "forty"
dec 5 = "fifty"
dec 6 = "sixty"
dec 7 = "seventy"
dec 8 = "eighty"
dec 9 = "ninety"
dec n = unsafeCrashWith ("unhandled number in dec:" <> show n)

w2:: Int -> String
w2 n | n<20 = w1 n
w2 n | n<100 && mod n 10 == 0 = dec (n/10)
w2 n | n<100 = dec (n/10) <> " " <> w1 (mod n 10)
w2 n | n<1000 && mod n 100 == 0 = w1 (n/100) <> " hundred"
w2 n | n<1000 = w1 (n/100) <> " hundred and " <> w2 (mod n 100)
w2 1000 = "one thousend"
w2 n = unsafeCrashWith ("unhandled number in w2:" <> show n)

countChars:: String -> Int
countChars s = replaceAll (Pattern " ") (Replacement "") s # length

e17:: Unit -> Int
e17 unit = map (w2 >>> countChars) (1..1000) # sum
