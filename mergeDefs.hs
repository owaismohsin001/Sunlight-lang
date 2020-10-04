module MergeDefs where

import Parser

mergeMultipleNode (ProgramNode ps pos) = 
    ProgramNode (foldr (++) [] $ map merge ps) pos where
        merge (MultipleDefinitionNode mds) = mds
        merge n = [n]
