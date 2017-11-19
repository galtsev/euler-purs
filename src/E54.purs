module E54 where

import Prelude
import Data.Maybe (Maybe(..))
import Data.List (List(..),(:),sort)
import Data.Foldable (all)
import Partial.Unsafe (unsafeCrashWith)

import E54Types (Card(..), Hand, Rank(..), Value, seq)

data Gr = Empty | Gr Value Int
type Groups = {c2:: List Value, c3:: Maybe Value, c4:: Maybe Value}

group:: Hand -> Groups
group sh = let
        gr st cc Nil = merge st cc
        gr st (Gr val cnt) ((Card _ vx):xs) | val==vx = gr st (Gr val (cnt+1)) xs
        gr st cc ((Card _ vx):xs) = gr (merge st cc) (Gr vx 1) xs

        merge st (Gr val 2) = st {c2=val:st.c2}
        merge st (Gr val 3) = st {c3=Just val}
        merge st (Gr val 4) = st {c4=Just val}
        merge st _ = st

    in gr {c2: Nil, c3: Nothing, c4: Nothing} Empty sh

straight:: Hand -> Maybe Value
straight Nil = unsafeCrashWith "straight on Nil"
straight ((Card _ v):xs) = ss v xs
    where
        ss val Nil = Just val
        ss prev ((Card _ val):vs) | seq prev val = ss val vs
        ss _ _ = Nothing

first:: Hand -> Card
first (x:xs) = x
first Nil = unsafeCrashWith "first on nil"

flush:: Hand -> Boolean
flush h = let (Card suit val) =  first h in all (\(Card s _)-> s==suit) h

-- last element of sorted list have biggest value
high:: Hand -> Value
high ((Card _ val):Nil) = val
high (x:xs) = high xs
high Nil = unsafeCrashWith "high on Nil"


rank:: Hand -> Rank
rank h = let
        sh = sort h
        fl = flush sh
        r1:: Groups -> Maybe Value -> Rank
        r1 _ (Just ace) | fl = RoyalFlush
        r1 _ (Just val) | fl = StraightFlush val
        r1 {c4:Just val} _ = Four val
        r1 {c2:(v2:Nil), c3:Just v3} _ = FullHouse v3 v2
        r1 _ _ | fl = Flush
        r1 _ (Just val) = Straight val
        r1 {c3:Just val} _ = Three val
        r1 {c2:(v:v1:Nil)} _ = if v>v1 then TwoPairs v v1 else TwoPairs v1 v
        r1 {c2:(val:Nil)} _ = Pair val
        r1 _ _ = High (high sh)
    in r1 (group sh) (straight sh)
