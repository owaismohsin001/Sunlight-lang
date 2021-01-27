module Nodes where

import Data.List
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char

data Node =
    StringNode String SourcePos
    | NumNode String SourcePos
    | IdentifierNode String SourcePos
    | BinOpNode Node String Node SourcePos
    | UnaryExpr String Node SourcePos
    | IfNode Node Node (Maybe Node) SourcePos
    | SequenceIfNode [Node] SourcePos
    | ProgramNode [Node] SourcePos
    | ListNode [Node] SourcePos
    | TupleNode [Node] SourcePos
    | CallNode Node [Node] SourcePos
    | DeclNode Node Node SourcePos
    | FuncDefNode (Maybe Node) [Node] Node SourcePos
    | MethodNode Node [Node] SourcePos
    | NewMethodNode Node Node Node SourcePos
    | StructInstanceNode Node [Node] Bool SourcePos
    | StructDefNode Node [Node] Bool (Maybe Node) SourcePos
    | SumTypeNode [Node] SourcePos
    | DeStructure [Node] SourcePos
    | DataNode String SourcePos
    | BoolNode String SourcePos
    | FromStruct Node
    | WhereNode Node [Node] SourcePos
    | StrictNode Node
    | MultipleDefinitionNode [Node]
    | ExternalNode Node [Node] SourcePos
    | TypeRefNode Node SourcePos
    deriving (Eq)

instance Show Node where
    show (StringNode str _) = "\"" ++ str ++ "\""
    show (SequenceIfNode ifs _) = "[" ++ intercalate ", " (map show ifs) ++ "]"
    show (NumNode n _) = n
    show (IdentifierNode id _) = id
    show (CallNode callee args _) = show callee ++ "(" ++ intercalate ", " (map show args) ++ ")"
    show (WhereNode e ds _) = show e ++ "{" ++ intercalate ", " (map show ds) ++"}"
    show (MultipleDefinitionNode ds) = "start:\n" ++ (intercalate "\n" (map show ds)) ++ "\nend"
    show (ProgramNode arr _) = intercalate "\n" (map show arr)
    show (ListNode xs _) = "[" ++ intercalate ", " (map show xs) ++ "]"
    show (TupleNode arr _) = "(" ++ intercalate ", " (map show arr) ++ ",)"
    show (BinOpNode a op b _) = "(" ++ show a ++ op ++ show b ++ ")"
    show (IfNode ce te ee _) = 
        "if " ++ show ce ++ " then " ++ show te ++ maybe "" (\e -> " else " ++ show e) ee
    show (DeclNode lhs rhs _) = "let " ++ show lhs ++ " <- " ++ show rhs
    show (BoolNode b _) = b
    show (UnaryExpr u n _) = "(" ++ u ++ " " ++ show n ++ ")"
    show (DataNode n _) = n
    show (FromStruct n) = "FromStruct: " ++ show n
    show (TypeRefNode e _) = "&" ++ show e
    show (StructDefNode id ls st ov _) = isStrict st ++ show id ++ "{" ++ intercalate "; " (map show ls) ++ "}" ++ decCase 
        where
            decCase =
                case ov of
                    Just a -> " -> " ++ show a
                    Nothing -> ""
            isStrict a = if a then "!" else ""

    show (SumTypeNode ds _) = intercalate " | " (map show ds)
    show (StructInstanceNode id ls _ _) = show id ++ "{" ++ intercalate "; " (map show ls) ++ "}"
    show (DeStructure ds _) = "{" ++ intercalate ", " (map show ds) ++ "}"
    show (MethodNode id args _) = "open " ++ show id ++ "(" ++ intercalate ", " (map show args) ++ ")"
    show (ExternalNode id ds pos) = "external " ++ show id ++ "{" ++ intercalate ", " (map show ds) ++ "}"
    show (NewMethodNode id ce te _) = 
        "close " ++ show id ++ " ? " ++ show ce ++ " -> " ++ show te
    show (FuncDefNode f args e _) = fname ++ " <- " ++ "(" ++ intercalate ", " (map show args) ++ ")" ++ " -> " ++ show e where
        fname =
            case f of
                Just f -> show f
                Nothing -> "anonymous"

extractString (StringNode str _) = str
extractString (NumNode num _) = num
extractString (IdentifierNode ident _) = ident
extractString (DataNode ident _) = ident

extractList (ListNode ls _) = ls
extractList (ProgramNode ls _) = ls

extractDecl (IdentifierNode id _) = id
extractStructInstance (StructInstanceNode id ls _ _) = (id, ls)
