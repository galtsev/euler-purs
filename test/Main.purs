module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.Assert (ASSERT)

-- import Test.Euler (testEuler)
-- import Test.E21 as E21
-- import Test.E22 as E22
-- import Test.E23 as E23
-- import Test.E24 (testMe)
-- import Test.E26 (testMe)
-- import Test.E27 (testMe)
-- import Test.E30 (testMe)
-- import Test.E31 (testMe)
-- import Test.E32 (testMe)
-- import Test.E35 (testMe)
-- import Test.E37 (testMe)
-- import Test.E38 (testMe)
import Test.E40 (testMe)




main :: forall e. Eff (assert :: ASSERT, console :: CONSOLE | e) Unit
main = do
  log "Start tests"
  -- testEuler
  -- E21.testMe
  -- E22.testMe
  -- E23.testMe
  testMe
  log "Tests done"
