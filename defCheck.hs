module DefCheck where

import Data.Void
import qualified Data.Map as Map
import Data.List
import Nodes
import Debug.Trace
import Data.Hashable
import ReduceMethods
import EitherUtility
import Data.Maybe
import Scope
import qualified Data.Set as Set
import qualified Text.Megaparsec as P

-- Get a list of duplicates from a list of things
repeated :: Ord a => [a] -> [a]
repeated = repeatedBy (>1) where
    repeatedBy p = map head . filterByLength p
    filterByLength p = filter (p . length) . sg
    sg = group . sort

-- Define all top-level definitions
define :: [StringPos] -> Node -> [StringPos]
define ds (IdentifierNode id pos) = ds ++ [StringPos id pos]
define ds (DataNode id pos) = ds ++ [StringPos id pos]
define ds (TupleNode t _) = concatMap (define ds) t
define ds (ProgramNode ns _) = concatMap (define ds) ns
define ds (DeclNode lhs _ _) = define ds lhs
define ds (StructDefNode id _ _ (Just (DataNode o dtpos)) pos) = define ds id ++ [StringPos o dtpos]
define ds (StructDefNode id _ _ Nothing _) = define ds id
define ds (DeStructure dcs _) = concatMap (define ds) dcs
define ds (SumTypeNode (dc:dcs) pos) = 
    define ds dc ++ concatMap (define ds) (map (\(StructDefNode id args b ov pos) -> StructDefNode id args b Nothing pos) dcs)
define ds (SumTypeNode a _) = define ds (head a)
define ds (MethodNode id _ _) = define ds id 
define ds NewMethodNode{} = ds

-- Define function arguments and where clauses
define ds (FuncDefNode _ args _ _) = ds ++ concatMap (define ds) args
define ds (WhereNode _ dcs _) = ds ++ concatMap (define ds) dcs
define ds a = error(show a)

-- Run definition checker
runDefiner :: Either String Node -> Maybe Scope -> Either String (Node, Scope)
runDefiner le@(Left e) parent = Left e
runDefiner (Right n) parent = 
    case repeated defs of
        [] -> Right (n, Scope (Set.fromList defs) parent)
        ls -> Left $ intercalate "\n\n" $ map (\(StringPos str pos) -> "Duplicate definition " ++ str ++ "\n" ++ showPos pos) ls
    where
        defs = define [] n ++ 
            case parent of 
                Just _ -> []
                Nothing -> zipWith StringPos baseSymbols (map (const pos) baseSymbols)
        posFromProgram (ProgramNode _ pos) = pos
        pos = posFromProgram n
        baseSymbols = [
            "listHead", "listTail", "SltList", "SltNum", "eval",
            "SltString", "SltTuple", "SltBool", "SltFunc",
            "unsafeMod", "baseStringify", "unsafeWrite", "unsafeRead"
            ]

ioDefiner :: Either (P.ParseErrorBundle String Data.Void.Void) Node -> Maybe Scope -> IO ()
ioDefiner fa b =
        case runDefiner a b of
            Left e -> putStrLn e
            Right (_, a) -> print a 
        where 
            a :: Either String Node
            a = case fa of
                    Left e -> Left $ P.errorBundlePretty e
                    Right n -> Right n

-- Check every used variable is defined
isDefined :: Scope -> Node -> Either String ()
isDefined sc (ProgramNode dcs _) = verify $ map (isDefined sc) dcs
isDefined sc (DeclNode lhs rhs pos) = isDefined sc rhs
isDefined sc (BinOpNode lhs op rhs pos) = 
    case op of
        "." -> isDefined sc lhs
        _ -> isDefined sc lhs |>> isDefined sc rhs
isDefined sc (IdentifierNode id pos) = StringPos id pos `exists` sc
isDefined sc n@(FuncDefNode mid args expr pos) = 
    expSc |>> 
        case mid of 
            Just id -> 
                isDefined sc id
            Nothing -> Right ()
    where
        expSc = 
            case runDefiner (Right n) $ Just sc of
                Left s -> Left s
                Right (_, nsc) -> isDefined nsc expr
isDefined sc n@(WhereNode expr ds pos) = 
    case runDefiner (Right n) $ Just sc of
        Left s -> Left s
        Right (_, nsc) -> isDefined nsc expr
isDefined sc (CallNode id args pos) = verify $ isDefined sc id : map (isDefined sc) args
isDefined sc (UnaryExpr _ e _) = isDefined sc e
isDefined sc (IfNode ce te ee _) = isDefined sc ce |>> isDefined sc te |>> 
    case ee of
        Just e -> isDefined sc e
        Nothing -> Right ()
isDefined sc (SequenceIfNode ns _ pos) = verify $ map (isDefined $ newSc sc) ns where
    newSc sc = Scope {getElems = Set.singleton $ StringPos "def" pos, getParent = Just sc}
isDefined sc (ListNode ns _) = verify $ map (isDefined sc) ns
isDefined sc (TupleNode ts _) = verify $ map (isDefined sc) ts
isDefined sc (TypeRefNode e _) = isDefined sc e
isDefined sc (StructInstanceNode id args _ _) = 
    isDefined sc id |>> verify (map (isDefined sc) args)
isDefined sc (StructDefNode id args _ mov _) = 
    case mov of
        Nothing -> Right ()
        Just ov -> isDefined sc ov
isDefined sc SumTypeNode{} = Right ()
isDefined sc DeStructure{} = Right ()
isDefined sc (DataNode id pos) = StringPos id pos `exists` sc
isDefined sc (NewMethodNode id _ _ _) = Left $ "Undefined open method " ++ show id
isDefined _ p = 
    case p of 
        NumNode _ _ -> Right ()
        StringNode _ _ -> Right ()
        BoolNode _ _ -> Right ()
        a -> Left $ show a

-- Take a set of sets and reduce it down to a set
reduceSets :: Set.Set StringPos -> [Node] -> Set.Set StringPos
reduceSets st dcs = Set.foldr Set.union Set.empty $ Set.fromList $ map (usedVars st) dcs

-- Collect a set of all used variable names to compare them with unused ones
usedVars :: Set.Set StringPos -> Node -> Set.Set StringPos
usedVars st (ProgramNode dcs _) = st `reduceSets` dcs
usedVars st (DeclNode lhs rhs _) = usedVars st rhs `Set.union` st
usedVars st (BinOpNode lhs op rhs pos) = 
    case op of
        "." -> usedVars st lhs `Set.union` st
        _ -> usedVars st lhs `Set.union` (usedVars st rhs `Set.union` st)
usedVars st (IdentifierNode id pos) = st `Set.union` Set.singleton (StringPos id pos) `Set.union` Set.singleton (StringPos "unsafeRunIO" pos)
usedVars st n@(FuncDefNode _ args expr pos) = 
    usedVars st expr `Set.difference` (Set.empty `reduceSets` args)
usedVars st n@(WhereNode expr ds pos) = usedVars st expr `Set.union` (st `Set.union` (Set.empty `reduceSets` ds))
usedVars st (CallNode id args pos) = usedVars st id `Set.union` (st `reduceSets` args)
usedVars st (UnaryExpr _ e _) = usedVars st e
usedVars st (TypeRefNode e _) = usedVars st e
usedVars st (IfNode ce te ee _) = usedVars st ce `Set.union` usedVars st te `Set.union` 
    case ee of
        Just e -> usedVars st e
        Nothing -> Set.empty
usedVars st (SequenceIfNode ns _ _) =  st `reduceSets` ns
usedVars st (ListNode ns _) = st `reduceSets` ns
usedVars st (TupleNode ts _) = st `reduceSets` ts
usedVars st (StructInstanceNode id args _ _) = 
    usedVars st id `Set.union` (st `reduceSets` args)
usedVars st (StructDefNode id args _ mov _) = 
    case mov of
        Nothing -> st
        Just ov -> usedVars st ov
usedVars st SumTypeNode{} = st
usedVars st DeStructure{} = st
usedVars st (DataNode id pos) = st `Set.union` Set.singleton (StringPos id pos)
usedVars _ p = 
    case p of 
        NumNode _ _ -> Set.empty
        StringNode _ _ -> Set.empty
        BoolNode _ _ -> Set.empty
        a -> Set.empty

-- Remove def keyword
removeDefKey :: Node -> Node
removeDefKey (ProgramNode ds pos) = ProgramNode (map removeDefKey ds) pos 
removeDefKey (DeclNode lhs rhs pos) = DeclNode lhs (removeDefKey rhs) pos
removeDefKey (BinOpNode lhs op rhs pos) = 
    case op of
        "." -> BinOpNode (removeDefKey lhs) op rhs pos
        _ -> BinOpNode (removeDefKey lhs) op (removeDefKey rhs) pos
removeDefKey fid@(IdentifierNode id pos) = if id == "def" then BoolNode "true" pos else fid
removeDefKey n@(FuncDefNode mid args expr pos) = FuncDefNode mid args (removeDefKey expr) pos
removeDefKey n@(WhereNode expr ds pos) = WhereNode (removeDefKey expr) (map removeDefKey ds) pos
removeDefKey (CallNode id args pos) = CallNode (removeDefKey id) (map removeDefKey args) pos
removeDefKey (UnaryExpr op e pos) = UnaryExpr op (removeDefKey e) pos
removeDefKey (IfNode ce te ee pos) = IfNode (removeDefKey ce) (removeDefKey te) fee pos where
    fee =
        case ee of
            Nothing -> Nothing
            Just n -> Just $ removeDefKey n
removeDefKey (SequenceIfNode ns mels pos) = SequenceIfNode (map removeDefKey ns) (getElse mels) pos where
    getElse (Just a) = Just $ removeDefKey a
    getElse Nothing = Nothing
removeDefKey (ListNode ns pos) = ListNode (map removeDefKey ns) pos
removeDefKey (TupleNode ts pos) = TupleNode (map removeDefKey ts) pos
removeDefKey (StructInstanceNode id args lazy pos) = StructInstanceNode (removeDefKey id) (map removeDefKey args) lazy pos
removeDefKey st@(StructDefNode id args strct mov pos) = 
    case mov of
        Nothing -> st
        Just ov -> StructDefNode id args strct (Just $ removeDefKey ov) pos
removeDefKey n@SumTypeNode{} = n
removeDefKey n@DeStructure{} = n
removeDefKey fid@DataNode{} = fid
removeDefKey (NewMethodNode id cond exp pos) = NewMethodNode (removeDefKey id) (removeDefKey cond) (removeDefKey exp) pos
removeDefKey p = p

-- Define all struct definitions
defStruct :: [(StringPos, [String])] -> Node -> [(StringPos, [String])]
defStruct ds (ProgramNode ns _) = concatMap (defStruct ds) ns
defStruct ds (StructDefNode (DataNode id dpos) args _ _ pos) = [(StringPos id dpos, map extractString args)]
defStruct ds (SumTypeNode (dcs) pos) = concatMap (defStruct ds) dcs
defStruct ds _ = ds

-- Check all structs have correct arguments
checkStructArgs :: Map.Map StringPos [String] -> Node -> Either String ()
checkStructArgs sc (ProgramNode dcs _) = verify $ map (checkStructArgs sc) dcs
checkStructArgs sc (DeclNode lhs rhs _) = checkStructArgs sc rhs
checkStructArgs sc (BinOpNode lhs op rhs pos) = 
    case op of
        "." -> checkStructArgs sc lhs
        _ -> checkStructArgs sc lhs |>> checkStructArgs sc rhs
checkStructArgs sc n@(FuncDefNode _ _ expr _) = checkStructArgs sc expr
checkStructArgs sc n@(WhereNode expr _ _) = checkStructArgs sc expr
checkStructArgs sc (CallNode id args pos) = verify $ checkStructArgs sc id : map (checkStructArgs sc) args
checkStructArgs sc (UnaryExpr _ e _) = checkStructArgs sc e
checkStructArgs sc (IfNode ce te ee _) = checkStructArgs sc ce |>> checkStructArgs sc te |>> 
    case ee of
        Just e -> checkStructArgs sc e
        Nothing -> Right ()
checkStructArgs sc (SequenceIfNode ns _ _) = verify $ map (checkStructArgs sc) ns
checkStructArgs sc (ListNode ns _) = verify $ map (checkStructArgs sc) ns
checkStructArgs sc (TupleNode ts _) = verify $ map (checkStructArgs sc) ts
checkStructArgs sc (StructInstanceNode (DataNode id ipos) args _ pos) = 
    case margs of
        Just eargs -> 
            if getDecls args == (eargs :: [String]) then Right ()
            else Left $ 
                showPos pos ++ "\nIn struct " ++ id ++ " expected " ++ formatList eargs ++ ", but got " ++ formatList (getDecls args)
        Nothing -> Left $ showPos pos ++ "\n" ++ "Undefined struct " ++ id
    where 
        formatList ls = "{" ++ intercalate ", " ls ++ "}"
        margs = Map.lookup (StringPos id pos) sc
        getDecls ls = map (\(DeclNode lhs rhs pos) -> extractString lhs) ls 
checkStructArgs sc StructDefNode{} = Right()
checkStructArgs sc SumTypeNode{} = Right ()
checkStructArgs sc DeStructure{} = Right ()
checkStructArgs sc TypeRefNode{} = Right ()
checkStructArgs _ p = 
    case p of 
        NumNode _ _ -> Right ()
        StringNode _ _ -> Right ()
        BoolNode _ _ -> Right ()
        IdentifierNode _ _ -> Right ()
        DataNode _ _ -> Right ()
        a -> error (show a)


-- Run the definition checker
checkDefinitions :: Either String Node -> Maybe Scope -> Either String Node
checkDefinitions le parent =
    case runDefiner le parent of
        Left e -> Left e
        Right (n, sc) -> 
            case runMethodFun sc (Right n) of
                Left e -> Left e
                Right nn -> 
                    case isDefined sc rmn  of
                        Left s -> Left s
                        Right () -> 
                            case (StringPos "out" (getStringPos nn) `exists` sc) of 
                                Left _ -> Left "No entry point defined"
                                Right () -> 
                                    case checkStructArgs (Map.fromList $ defStruct [] filteredRmns) filteredRmns of
                                        Right () -> Right $ filteredRmns
                                        Left a -> Left a
                                    where filteredRmns = outputOut sc $ removeDefKey $ filterDefs rmn
                    where 
                        outputOut sc (ProgramNode ps pos) = ProgramNode (map (outputOut sc) ps) pos
                        outputOut sc nd@(DeclNode id@(IdentifierNode "out" ipos) def pos) = 
                            case (StringPos "unsafeRunIO" (getStringPos nn) `exists` sc) of 
                                Left a -> DeclNode id (CallNode (IdentifierNode "unsafeWrite" ipos) [def] pos) pos
                                Right () -> DeclNode id (CallNode (IdentifierNode "unsafeRunIO" ipos) [def] pos) pos
                        outputOut _ a = a

                        defStructs n = defStruct [] n
                        rmn = removeNewMethods sc nn
                        filterDefs n@(ProgramNode dfs pos) = 
                            let used = usedVars Set.empty n in 
                                ProgramNode (filter (toKeep used) dfs) pos
                        toKeep used x = 
                            head (define [] x) `Set.member` used || str == "out" || str == "loopout" where 
                                str = getPosString $ head $ define [] x
                        getPosString (StringPos s _) = s
                        getStringPos (ProgramNode _ pos) = pos

checkIODefinitions :: Either (P.ParseErrorBundle String Data.Void.Void) Node -> Maybe Scope -> IO ()
checkIODefinitions lf p =
    case checkDefinitions l p of 
        Left e -> putStrLn e
        Right _ -> putStrLn "All variables are defined"
    where l = case lf of
                Left e -> Left $ P.errorBundlePretty e
                Right n -> Right n