module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.Assert (ASSERT)

-- import Test.Euler (testMe)
import Test.E43 (testMe)




main :: forall e. Eff (assert :: ASSERT, console :: CONSOLE | e) Unit
main = do
  log "Start tests"
  -- testEuler
  -- E21.testMe
  -- E22.testMe
  -- E23.testMe
  testMe
  log "Tests done"
