module E54Types where

import Prelude
import Data.List (List(..))

data Suit = Diamond | Club | Heart | Spade

derive instance eqSuit:: Eq Suit
instance showSuit:: Show Suit where
    show Diamond = "Diamond"
    show Club = "Club"
    show Heart = "Heart"
    show Spade = "Spade"


newtype Value = Value Int

derive instance eqValue:: Eq Value
derive instance ordValue:: Ord Value
instance showValue:: Show Value where
    show (Value v) | v>1 && v<11 = show v
    show (Value 11) = "Jack"
    show (Value 12) = "Queen"
    show (Value 13) = "King"
    show (Value 14) = "Ace"
    show (Value v) = "value?<" <> show v <> ">"

jack:: Value
jack = Value 11
queen:: Value
queen = Value 12
king:: Value
king = Value 13
ace:: Value
ace = Value 14

seq:: Value -> Value -> Boolean
seq (Value a) (Value b) = b-a==1


class CharRepr a where
    charFor:: a -> String


instance charReprSuit:: CharRepr Suit where
    charFor Diamond = "D"
    charFor Club = "C"
    charFor Heart = "H"
    charFor Spade = "S"

instance charReprValue:: CharRepr Value where
    charFor (Value 2) = "2"
    charFor (Value 3) = "3"
    charFor (Value 4) = "4"
    charFor (Value 5) = "5"
    charFor (Value 6) = "6"
    charFor (Value 7) = "7"
    charFor (Value 8) = "8"
    charFor (Value 9) = "9"
    charFor (Value 10) = "0"
    charFor (Value 11) = "J"
    charFor (Value 12) = "Q"
    charFor (Value 13) = "K"
    charFor (Value 14) = "A"
    charFor _ = "?"

data Card = Card Suit Value

instance eqCard:: Eq Card where
    eq (Card s1 v1) (Card s2 v2) = s1==s2 && v1==v2

instance ordCard:: Ord Card where
    compare (Card _ v1) (Card _ v2) = compare v1 v2


instance showCard:: Show Card where
    show (Card suit value) = (charFor suit) <> (charFor value)


-- | High Card: Highest value card.
-- | One Pair: Two cards of the same value.
-- | Two Pairs: Two different pairs.
-- | Three of a Kind: Three cards of the same value.
-- | Straight: All cards are consecutive values.
-- | Flush: All cards of the same suit.
-- | Full House: Three of a kind and a pair.
-- | Four of a Kind: Four cards of the same value.
-- | Straight Flush: All cards are consecutive values of same suit.
-- | Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.

data Rank = High Value
    | Pair Value
    | TwoPairs Value Value
    | Three Value
    | Straight Value
    | Flush
    | FullHouse Value Value
    | Four Value
    | StraightFlush Value
    | RoyalFlush

intRank:: Rank -> Int
intRank (High _) = 0
intRank (Pair _) = 1
intRank (TwoPairs _ _) = 2
intRank (Three _) = 3
intRank (Straight _) = 4
intRank (Flush) = 5
intRank (FullHouse _ _) = 6
intRank (Four _) = 7
intRank (StraightFlush _) = 8
intRank (RoyalFlush) = 9


instance eqRank:: Eq Rank where
    eq (High a) (High b) = a==b
    eq (Pair a) (Pair b) = a==b
    eq (TwoPairs a b) (TwoPairs a1 b1) = a==a1 && b==b1
    eq (Three a) (Three b) = a==b
    eq (Straight a) (Straight b) = a==b
    eq Flush Flush = true
    eq (FullHouse a b) (FullHouse a1 b1) = a==a1 && b==b1
    eq (Four a) (Four b) = a==b
    eq (StraightFlush a) (StraightFlush b) = a==b
    eq RoyalFlush RoyalFlush = true
    eq _ _ = false

instance ordRank:: Ord Rank where
    compare (High a) (High b) = compare a b
    compare (Pair a) (Pair b) = compare a b
    compare (TwoPairs a b) (TwoPairs a1 b1) | a==a1 = compare b b1
    compare (TwoPairs a b) (TwoPairs a1 b1) = compare a a1
    compare (Three a) (Three b) = compare a b
    compare (Straight a) (Straight b) = compare a b
    compare Flush Flush = EQ
    compare (FullHouse a b) (FullHouse a1 b1) | a==a1 = compare b b1
    compare (FullHouse a b) (FullHouse a1 b1) = compare a b
    compare (Four a) (Four b) = compare a b
    compare (StraightFlush a) (StraightFlush b) = compare a b
    compare RoyalFlush RoyalFlush = EQ
    compare a b = compare (intRank a) (intRank b)

type Hand = List Card
