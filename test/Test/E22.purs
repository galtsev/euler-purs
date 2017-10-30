module Test.E22 where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.Assert (ASSERT, assert)

import E22 (unquote,parseNames,score)

testMe:: forall e. Eff (assert :: ASSERT, console :: CONSOLE | e) Unit
testMe = do
    log "start E22"
    assert $ unquote "\"hello\"" == "hello"
    assert $ parseNames """ "1ONE","3three","2two" """ == ["1one","2two","3three"]

    assert $ score "aabc" == (1+1+2+3)
    assert $ score "COLIN" == 53