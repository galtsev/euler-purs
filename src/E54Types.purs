module E54Types where

import Prelude
import Data.List (List,fromFoldable)
import Partial.Unsafe (unsafeCrashWith)
import Data.String as S

data Suit = Diamond | Club | Heart | Spade

derive instance eqSuit:: Eq Suit
instance showSuit:: Show Suit where
    show Diamond = "Diamond"
    show Club = "Club"
    show Heart = "Heart"
    show Spade = "Spade"

parseSuit:: String -> Suit
parseSuit "D" = Diamond
parseSuit "C" = Club
parseSuit "H" = Heart
parseSuit "S" = Spade
parseSuit s = unsafeCrashWith ("error parsing suit " <> s)

newtype Value = Value Int

derive instance eqValue:: Eq Value
derive instance ordValue:: Ord Value
instance showValue:: Show Value where
    show (Value v) | v>1 && v<11 = "V" <> show v
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

parseValue:: String -> Value
parseValue s = p s
    where
        p "2" = Value 2
        p "3" = Value 3
        p "4" = Value 4
        p "5" = Value 5
        p "6" = Value 6
        p "7" = Value 7
        p "8" = Value 8
        p "9" = Value 9
        p "T" = Value 10
        p "J" = Value 11
        p "Q" = Value 12
        p "K" = Value 13
        p "A" = Value 14
        p val = unsafeCrashWith ("error parsing value " <> val)


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
    charFor (Value 10) = "T"
    charFor (Value 11) = "J"
    charFor (Value 12) = "Q"
    charFor (Value 13) = "K"
    charFor (Value 14) = "A"
    charFor (Value val) = unsafeCrashWith ("error in charFor value " <> show val)

data Card = Card Suit Value

instance eqCard:: Eq Card where
    eq (Card s1 v1) (Card s2 v2) = s1==s2 && v1==v2

instance ordCard:: Ord Card where
    compare (Card _ v1) (Card _ v2) = compare v1 v2

instance showCard:: Show Card where
    show (Card suit value) = (charFor suit) <> (charFor value)

parseCard:: String -> Card
parseCard s = let
        vch = S.take 1 s
        sch = S.take 1 (S.drop 1 s)
    in Card (parseSuit sch) (parseValue vch)

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

instance showRank:: Show Rank where
    show (High v) = "High<" <> show v <> ">"
    show (Pair v) = "Pair<" <> show v <> ">"
    show (TwoPairs a b) = "TwoPairs<" <> show a <> "," <> show b <> ">"
    show (Three v) = "Three<" <> show v <> ">"
    show (Straight v) = "Straight<" <> show v <> ">"
    show Flush = "Flush"
    show (FullHouse a b) = "FullHouse<" <> show a <> "," <> show b <> ">"
    show (Four v) = "Four<" <> show v <> ">"
    show (StraightFlush v) = "StraightFlush<" <> show v <> ">"
    show RoyalFlush = "RoyalFlush"


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

parseHand:: String -> Hand
parseHand s = S.split (S.Pattern " ") s # map parseCard # fromFoldable