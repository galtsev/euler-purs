module E9 where

import Prelude
import Data.List (List,(..))
import Control.MonadZero (guard)


-- | a^2+b^2==c^2
-- | a+b+c==1000
-- | a < b < c
-- | so min(a)==1, min(b)==2, min(c)==3, max(c)==997
e9:: Unit -> List Int
e9 unit = do
	c <- 3..997
	let ab = 1000 - c
	b <- 2..(c-1)
	let a = ab - b
	guard $ a>0 && a*a+b*b==c*c
	pure (a*b*c)
