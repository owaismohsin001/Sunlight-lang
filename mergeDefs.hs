module MergeDefs where

import Parser

mergeMultipleNode (ProgramNode ps pos) = 
    ProgramNode (foldr (++) [] $ map merge ps) pos where
        merge :: Node -> [Node]
        merge (MultipleDefinitionNode mds) = foldr (++) [] $ map merge mds
        merge (FromStruct n) = [n]
        merge n = [n]
