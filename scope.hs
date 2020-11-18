module Scope where

import Data.Hashable
import qualified Data.Set as Set
import qualified Text.Megaparsec as P
import Data.List
import Lev

import Debug.Trace

data Scope = Scope{getElems :: Set.Set StringPos, getParent :: Maybe Scope}

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

getAllElems :: Scope -> Set.Set StringPos
getAllElems sc = 
    case getParent sc of
        Just nsc -> getElems sc `Set.union` getAllElems nsc
        Nothing -> getElems sc

-- existence checking function
exists id sc =
    if id `existsIn` sc then 
        Right () 
    else 
        case closeOnes of
            [] -> Left $ showPos (getPos id) ++ "\n" ++ "No definition for '" ++ getStr id ++ "' found"
            [a] -> Left $ showPos (getPos id) ++ "\n" ++ "No definition for '" ++ getStr id ++ "' found\n" ++
                    "Maybe, you meant " ++ show a
            _ -> Left $ showPos (getPos id) ++ "\n" ++ "No definition for '" ++ getStr id ++ "' found\n" ++ 
                    "Perhaps, you meant one of " ++ intercalate ", " closeOnes
    where
        getPos (StringPos _ pos) = pos
        getStr (StringPos str _) = str
        closeOnes = closest 3 (getStr id) $ map getStr $ Set.toList $ getAllElems sc

-- showing a position
showPos (P.SourcePos s ln cn) = 
    "In file: " ++ s ++ ", at line: " ++ tail (dropWhile (/= ' ') (show ln)) ++ ", at colounm: " ++ tail (dropWhile (/= ' ') (show cn))