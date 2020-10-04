module Scope where

import Data.Hashable
import qualified Data.Set as Set
import qualified Text.Megaparsec as P
data Scope = Scope (Set.Set StringPos) (Maybe Scope)

existsIn str (Scope set par) = 
    (str `Set.member` set) ||
        case par of
            Just pset -> str `existsIn` pset
            Nothing -> False

instance Show Scope where
    show (Scope set par) = show set ++ " -> " ++ maybeScope where 
        maybeScope =
            case par of
                Just ( sc@Scope{} ) -> show sc
                Nothing -> "{}"

data StringPos = StringPos String P.SourcePos

instance Show StringPos where
    show (StringPos s _) = s

instance Eq StringPos where
    (StringPos a _) == (StringPos b _) = a == b

instance Ord StringPos where
    (StringPos a _) <= (StringPos b _) = hash a <= hash b

-- existence checking function
exists id sc =
    if id `existsIn` sc then 
        Right () 
    else 
        Left $ showPos (getPos id) ++ "\n" ++ "No definition for '" ++ getStr id ++ "' found"
    where
        getPos (StringPos _ pos) = pos
        getStr (StringPos str _) = str

-- showing a position
showPos (P.SourcePos s ln cn) = 
    "In file: " ++ s ++ ", at line: " ++ tail (dropWhile (/= ' ') (show ln)) ++ ", at colounm: " ++ tail (dropWhile (/= ' ') (show cn))