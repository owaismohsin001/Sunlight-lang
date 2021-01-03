module MergeDefs where

import Nodes

mergeMultipleNode (ProgramNode ps pos) = 
    ProgramNode (mconcat $ map merge ps) pos where
        merge :: Node -> [Node]
        merge (MultipleDefinitionNode mds) = mconcat $ map merge mds
        merge (FromStruct n) = [n]
        merge n = [n]


getDollar :: String -> Node -> Node
getDollar nm (DeclNode lhs rhs pos) = DeclNode lhs (getDollar nm rhs) pos
getDollar nm (BinOpNode lhs op rhs pos) = 
    case op of
        "." -> BinOpNode (getDollar nm lhs) op rhs pos
        _ -> BinOpNode (getDollar nm lhs) op (getDollar nm rhs) pos
getDollar nm fid@(IdentifierNode id pos) = if head id == '$' then IdentifierNode (nm ++ "__" ++ tail id) pos else fid
getDollar nm n@(FuncDefNode mid args expr pos) = FuncDefNode mid args (getDollar nm expr) pos
getDollar nm n@(WhereNode expr ds pos) = WhereNode (getDollar nm expr) (map (getDollar nm) ds) pos
getDollar nm (CallNode id args pos) = CallNode (getDollar nm id) (map (getDollar nm) args) pos
getDollar nm (UnaryExpr op e pos) = UnaryExpr op (getDollar nm e) pos
getDollar nm (IfNode ce te ee pos) = IfNode (getDollar nm ce) (getDollar nm te) fee pos where
    fee =
        case ee of
            Nothing -> Nothing
            Just n -> Just $ getDollar nm n
getDollar nm (SequenceIfNode ns pos) = SequenceIfNode (map (getDollar nm) ns) pos
getDollar nm (ListNode ns pos) = ListNode (map (getDollar nm) ns) pos
getDollar nm (TupleNode ts pos) = TupleNode (map (getDollar nm) ts) pos
getDollar nm (StructInstanceNode id args lazy pos) = StructInstanceNode (getDollar nm id) (map (getDollar nm) args) lazy pos
getDollar nm st@(StructDefNode id args strct mov pos) = 
    case mov of
        Nothing -> st
        Just ov -> StructDefNode id args strct (Just $ getDollar nm ov) pos
getDollar nm n@SumTypeNode{} = n
getDollar nm n@DeStructure{} = n
getDollar nm fid@(DataNode id pos) = if head id == '$' then DataNode (nm ++ "__" ++ tail id) pos else fid
getDollar nm (NewMethodNode id cond exp pos) = NewMethodNode (getDollar nm id) (getDollar nm cond) (getDollar nm exp) pos
getDollar nm (TypeRefNode n pos) = TypeRefNode (getDollar nm n) pos
getDollar _ p = p
