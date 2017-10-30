module TSeq where

import Prelude
import Data.Sequence (Seq,cons,empty)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

gen:: Int -> Maybe (Tuple Int Int)
gen 0 = Nothing
gen n = Just (Tuple n (n-1))


gseq:: Int -> Seq Int
gseq n = gs empty n
	where
		gs s 0 = s
		gs s x = gs (cons x s) (x-1)