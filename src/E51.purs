module E51 where

import Prelude
import Data.Foldable (any)
import Data.List (List(..),(:),range,filter,length)
import Euler (prime)

target:: Int
target = 8

-- | single-digit Int
type Digit = Int

-- | have form 10**N
type SimpleMutation = Int

-- | Complex mutation - number composed of 0 and 1
type Mutation = Int

-- | Mutation target - arbitrary Int
type Value = Int

-- | TODO - leading 0 as replacement slot?
-- | for every occurence of Digit (single-digit int) in Value at position M add 1eM (SimpleMutation) to output list
enumSimple:: Digit -> Value -> List SimpleMutation
enumSimple d val = en Nil val 1
    where
        en acc v _ | v==0 = acc
        en acc v p | mod v 10 == d = en (p:acc) (v/10) (p*10)
        en acc v p = en acc (v/10) (p*10)

-- | gen all compund mutations
mutationsFor:: Digit -> Value -> List Mutation
mutationsFor d val = gen (enumSimple d val)
    where
        gen Nil = Nil
        gen (x:Nil) = (x:Nil)
        gen (x:xs) = let xl = gen xs in xl <> x:map (x+_) xl

cmut:: Value -> Mutation -> List Int -> Int
cmut val mut idx = map (\i->val+mut*i) idx # filter prime # length

good:: Digit -> Value -> Boolean
good d val = any (\mut -> cmut val mut (range 0 (9-d))>=target) $ mutationsFor d val

ee0:: Value -> Value
ee0 val | not (prime val) = ee0 (val+1)
ee0 val | good 0 val = val
ee0 val | good 1 val = val
ee0 val | good 2 val = val
ee0 val = ee0 (val+1)

e51:: Unit -> Int
e51 unit = ee0 2