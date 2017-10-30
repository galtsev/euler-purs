module TDo where

import Prelude
import Data.List (List,(..))

-- 1*(1) 2*(1 2) 3*(1 2 3)= 1 2 4 3 6 9
square:: Int -> List Int
square x = do
	a <- 1..x
	b <- 1..a
	pure (a*b)
	
