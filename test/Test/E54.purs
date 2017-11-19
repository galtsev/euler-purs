module Test.E54 where

import Prelude
import Test.Testing (TestMe,log,assert)
import Data.Foldable (all)
import E54Types (Rank(..))

testMe:: forall e. TestMe e
testMe = do
    log "E54"
    
    assert $ all (_<RoyalFlush) [High, Pair, TwoPairs, Three, Straight, Flush, FullHouse, Four, StraightFlush]
    assert $ all (_<StraightFlush) [High, Pair, TwoPairs, Three, Straight, Flush, FullHouse, Four]
    assert $ all (_>Pair) [TwoPairs, Three, Straight, Flush, FullHouse, Four, StraightFlush, RoyalFlush]
    