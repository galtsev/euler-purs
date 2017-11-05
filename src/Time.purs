module Time where

foo:: Int
foo = 1
{-
timeIt:: forall a eff. (unit -> a) -> Eff(timer::TIMER|eff) Number
timeIt f = do
    t1 <- now
    let _ = f unit
    t2 <- now
    pure (t2-t1)
-}