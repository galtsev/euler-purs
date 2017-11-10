module E42 where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Data.String as S
import Data.Char as C
import Data.Array as A

parse:: String -> Array String
parse s = S.toUpper s # S.split (S.Pattern ",") # map (S.replaceAll (S.Pattern "\"") (S.Replacement ""))

a:: Int
a = C.toCharCode 'A'

wval:: String -> Int
wval s = S.toCharArray s # map (\c->C.toCharCode c - a + 1) # A.foldl (+) 0

triangle:: Int -> Int -> Int -> Boolean
triangle _ tn x | x==tn = true
triangle _ tn x | x<tn = false
triangle n _ x = let n1 = n+1 in triangle n1 (n1*(n1+1)/2) x

process:: String -> Int
process s = parse s # map wval # A.filter (triangle 1 1) # A.length

e42:: forall e. Eff (fs::FS,console::CONSOLE,exception::EXCEPTION | e) Int
e42 = do
    body <- readTextFile UTF8 "files/p042_words.txt"
    pure (process body)