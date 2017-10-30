module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.FS (FS)


-- import E21 (e21)
import E22 (e22)

main :: forall e. Eff (exception :: EXCEPTION, fs :: FS, console :: CONSOLE | e) Unit
main = do
  -- logShow (e21 unit)
  e22 unit
