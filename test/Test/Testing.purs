module Test.Testing (
    TestMe,
    module Control.Monad.Eff.Console,
    module Test.Assert,
    module Data.Foldable
    ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE,log)
import Test.Assert (ASSERT, assert)
import Data.Foldable (all)

type TestMe e = Eff (assert :: ASSERT, console :: CONSOLE | e) Unit