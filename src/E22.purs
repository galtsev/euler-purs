module E22 where

import Prelude
import Data.Maybe (Maybe(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Console(CONSOLE,log)
import Node.FS (FS)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)
import Data.String (split,Pattern(..),trim,toLower,toCharArray)
import Data.String.Regex (Regex,match)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.String.Regex.Flags(noFlags)
import Data.Array (sort)
import Data.Char (toCharCode)
import Partial.Unsafe (unsafeCrashWith)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Traversable (sum)

a:: Int
a = toCharCode('a')-1

score:: String -> Int
score s = toLower s # toCharArray # map (\c->toCharCode c - a) # sum

getRaw:: forall eff. Unit -> Eff (fs :: FS, exception :: EXCEPTION | eff) String
getRaw unit = readTextFile UTF8 "files/e22_names.txt"

unquoteRe:: Regex
unquoteRe = unsafeRegex "^\"(.*)\"$" noFlags

unquote:: String -> String
unquote name = let
        r0 (Just [_, Just v]) = v
        r0 _ = unsafeCrashWith ("wrong name:" <> name)
    in
        r0 (match unquoteRe name)

parseNames:: String -> Array String
parseNames raw = trim raw # split (Pattern ",") # map (trim >>> unquote >>> toLower) # sort

getScores:: String -> Int
getScores raw = parseNames raw # foldlWithIndex (\i acc name->acc+(i+1)*score name) 0

e22:: forall eff. Unit -> Eff (console::CONSOLE, fs :: FS, exception :: EXCEPTION | eff) Unit
e22 unit = do
    raw <- getRaw unit
    log (show (getScores raw))
