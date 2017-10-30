module E4 where

import Prelude
import Data.List as L
import Data.Array as A
import Data.String (toCharArray, fromCharArray)
import Partial.Unsafe (unsafePartial)
import Data.Maybe (fromJust)
import Data.Maybe.First (First(..))
import Data.Traversable (maximum)

revString:: String -> String
revString s = toCharArray s # L.fromFoldable # L.reverse # A.fromFoldable # fromCharArray

isPalindrome:: Int -> Boolean
isPalindrome v =
	let sv = show v
	in revString sv == sv

e4:: Unit -> Int
e4 unit =
	let
		n = 999
		v = do
			a <- L.range 1 n
			b <- L.range 1 n
			pure (a*b)
		v2 = L.filter isPalindrome v # maximum
	in
		unsafePartial (fromJust v2)

-- wrong result
e4':: Unit -> Int
e4' unit =
	let
		n = 999
		l1 = L.reverse (L.range 1 n)
		h v = L.reverse (L.range v n) # map (_*v) # L.filter isPalindrome # L.head
		r = map h l1 # L.foldMap First # \(First v) -> v
	in
		unsafePartial (fromJust r)
