module ReduceMethods where

import Parser hiding (True, False)
import Scope
import Data.Void
import EitherUtility
import Debug.Trace
import Data.List
import qualified Text.Megaparsec as P

-- A pass that turns methods into functions
methodFun p@(ProgramNode ps pos) = ProgramNode (getNodes $ map mFun ps) pos where
    mFun (MethodNode id args pos) = 
        Just $ DeclNode id (
            FuncDefNode 
                (Just id) 
                True 
                args 
                (SequenceIfNode (getNodes $ map (convMethod $ extractString id) ps) pos) pos
            ) pos where 
            ns = getNodes $ map (convMethod $ extractString id) ps
    mFun a = Just a

    getNodes :: [Maybe Node] -> [Node]
    getNodes xs = 
        map (\(Just a) -> a) (
            filter (
                \x ->
                    case x of
                        Just _ -> True
                        Nothing -> False
            ) xs
        )

    convMethod :: String -> Node -> Maybe Node
    convMethod rid (NewMethodNode (IdentifierNode id _) ce te pos) = if rid == id then Just $ IfNode ce te Nothing pos else Nothing
    convMethod _ _ = Nothing

--  Remove all the method nodes
removeNewMethods :: Scope -> Node -> Node
removeNewMethods sc (ProgramNode dcs pos) = ProgramNode ls pos where
    noNothings n = 
        case n of
            (NewMethodNode id _ _ pos) -> not $ StringPos (extractString id) pos `existsIn` sc
            n -> True

    ls = filter noNothings dcs

-- IO for method fun
runMethodFun :: Scope -> Either (P.ParseErrorBundle String Data.Void.Void) Node -> Either String Node
runMethodFun sc (Left e) = Left $ P.errorBundlePretty e
runMethodFun sc (Right n) = Right $ methodFun n
