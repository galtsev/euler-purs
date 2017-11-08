module Test.Testing (
    TestMe,
    module Control.Monad.Eff.Console,
    module Test.Assert
    ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE,log)
import Test.Assert (ASSERT, assert)

type TestMe e = Eff (assert :: ASSERT, console :: CONSOLE | e) Unit