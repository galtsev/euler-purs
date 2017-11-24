module Time where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW,now)
import Data.DateTime.Instant (Instant,unInstant)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))

toMs:: Instant -> Number
toMs v = let (Milliseconds ms) = unInstant v in ms

repeat:: forall a. Int -> (Unit -> a) -> a
repeat 1 f = f unit
repeat n f = let _ = f unit in repeat (n-1) f

timeIt:: forall a eff. Int -> (Unit -> a) -> Eff(now:: NOW | eff) (Tuple Number a)
timeIt cnt f = do
    t1 <- now
    let res = repeat cnt f
    t2 <- now
    pure (Tuple (toMs t2 - toMs t1) res)
