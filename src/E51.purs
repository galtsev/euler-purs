module E51 where

import Prelude
import Data.List (List(..),(:))

-- | single-digit Int
type Digit = Int

-- | have form 10**N
type SimpleMutation = Int

-- | Complex mutation - number composed of 0 and 1
type Mutation = Int

-- | Mutation target - arbitrary Int
type Value = Int

-- | for every occurence of Digit (single-digit int) in Value at position M add 1eM (SimpleMutation) to output list
enumSimple:: Digit -> Value -> List SimpleMutation
enumSimple d val = en Nil val 1
    where
        en acc v _ | v==0 = acc
        en acc v p | mod v 10 == d = en (p:acc) (v/10) (p*10)
        en acc v p = en acc (v/10) (p*10)

-- | gen all compund mutations
mutationsFor:: Digit -> Value -> List Mutation
mutationsFor d val = gen (enumSimple d val # reverse Nil)
    where
        gen Nil = Nil
        gen (x:xs) = let xl = gen xs in xl <> map (x+_) xl
