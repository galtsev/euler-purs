module Test.E54 where

import Prelude
import Test.Testing (TestMe,log,assert)
import Data.Foldable (all)
import Data.Array (range)
import Data.Tuple (Tuple(..))
import E54Types (Card(..),Suit(..),Value(..),Rank(..),parseCard,parseHand,jack,queen,king,ace)
import E54 (rank,parseGame,win)

cv:: Int -> Card
cv val = Card Club (Value val)

v2:: Value
v2 = Value 2

allVals:: Array Value
allVals = map Value (range 2 14)

allPairs:: (Value -> Value -> Boolean) -> Boolean
allPairs p = all (\val->all (p val) allVals) allVals

ph:: String -> Rank -> Boolean
ph s r = rank (parseHand s) == r

play:: String -> Ordering -> Boolean
play s o = let (Tuple hl hr) = parseGame s in win hl hr == o

testMe:: forall e. TestMe e
testMe = do
    log "E54"
    
    log "compare"
    assert $ all (RoyalFlush > _) (map StraightFlush allVals)
    assert $ allPairs (\a b-> StraightFlush a > Four b)
    assert $ allPairs (\a b->Four a > FullHouse b ace)
    assert $ allPairs (\a b->FullHouse a b > Flush)
    assert $ all (Flush > _) (map Straight allVals)
    assert $ allPairs (\a b->Straight a > Three b)
    assert $ allPairs (\a b->Three a > TwoPairs b king)
    assert $ allPairs (\a b->TwoPairs a b > Pair queen)
    assert $ allPairs (\a b->Pair a > High b)
    -- within same rank
    assert $ StraightFlush king > StraightFlush queen
    assert $ Four queen > Four (Value 9)
    assert $ FullHouse (Value 10) (Value 3) > FullHouse (Value 9) ace
    assert $ Straight jack > Straight (Value 8)
    assert $ Three queen > Three jack
    assert $ TwoPairs queen (Value 4) > TwoPairs queen (Value 3)
    assert $ TwoPairs queen (Value 2) > TwoPairs jack (Value 9)
    assert $ Pair king > Pair jack
    -- equals
    assert $ StraightFlush (Value 3) == StraightFlush (Value 3)
    assert $ Straight queen == Straight queen

    log "parsing"
    
    assert $ map parseCard ["2D","3C","4H","5S","6C","7C","8C","9C","TC","JC","QC","KC","AC"] ==
        [Card Diamond (Value 2), Card Club (Value 3), Card Heart (Value 4), Card Spade (Value 5)] <> map cv (range 6 14)
    
    log "ranking"
    assert $ ph "TC JC QC KC AC" RoyalFlush
    assert $ ph "4H 5H 6H 7H 8H" (StraightFlush (Value 8))
    assert $ ph "4D KD 4C 4H 4S" (Four (Value 4))
    assert $ ph "JD JH 3D 3C 3S" (FullHouse (Value 3) jack)
    assert $ ph "5D 4D 2D AD JD" Flush
    assert $ ph "8H 9H TC JC QD" (Straight queen)
    assert $ ph "2H 5C 5D QD 5S" (Three (Value 5))
    assert $ ph "2D 2S KS JS JC" (TwoPairs jack (Value 2))
    assert $ ph "KD 3C 4H 5S KS" (Pair king)
    assert $ ph "2D 3C 4H 6H QS" (High queen)

    log "check winners"
    assert $ play "5H 5C 6S 7S KD 2C 3S 8S 8D TD" LT
    assert $ play "5D 8C 9S JS AC 2C 5C 7D 8S QH" GT
    assert $ play "2D 9C AS AH AC 3D 6D 7D TD QD" LT
    assert $ play "4D 6S 9H QH QC 3D 6D 7H QD QS" GT
    assert $ play "2H 2D 4C 4D 4S 3C 3D 3S 9S 9D" GT
    -- from real file
    assert $ play "KH JS 4H 5D 9D TC TD QC JD TS" LT -- High king < Three 10
    assert $ play "QS QD AC AD 4C 6S 2D AS 3H KC" GT -- TwoPairs ace quin > High ace
    assert $ play "4C 7C 3C TD QS 9C KC AS 8D AD" LT -- High queen < Pair ace
    assert $ play "KC 7H QC 6D 8H 6S 5S AH 7S 8C" LT -- High king < High ace
    assert $ play "3S AD 9H JC 6D JD AS KH 6S JH" LT -- High ace < Pair jack
    assert $ play "AD 3D TS KS 7H JH 2D JS QD AC" LT -- High ace < Pair jack
    assert $ play "9C JD 7C 6D TC 6H 6C JC 3D 3S" LT -- High jack < TwoPairs 6 3
    assert $ play "QC KC 3S JC KD 2C 8D AH QS TS" GT -- Pair king > High ace
    assert $ play "AS KD 3D JD 8H 7C 8C 5C QD 6C" GT -- High ace > High queen
    