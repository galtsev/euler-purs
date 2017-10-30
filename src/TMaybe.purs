module TMaybe where

import Prelude
import Data.Maybe (Maybe)
import Data.Int as MInt
import Data.Number as MNum

class Parseable a where
	parse :: String -> Maybe a


instance parseableInt :: Parseable Int where
	parse = MInt.fromString

instance parseableNumber :: Parseable Number where
	parse = MNum.fromString


mysum:: Unit -> Maybe Int
mysum unit = do
	a <- parse "10"
	b <- parse "11"
	c <- parse "12"
	pure (a+b+c)

msum:: String -> String -> Maybe Int
msum s1 s2 = do
	a <- parse s1
	b <- parse s2
	pure (a+b)