module CodeGen where

import Parser
import qualified Text.Megaparsec as P
import Data.Maybe
import Data.List
import Data.List.Split
import Debug.Trace


-- Generate Forward declarations
fDeclare (IdentifierNode id _) = "local " ++ id ++ ";\n"
fDeclare (DataNode id _) = "local " ++ id ++ ";\n"
fDeclare (TupleNode t _) = intercalate "" $ map fDeclare t
fDeclare (ProgramNode ns _) = intercalate "" $ map fDeclare ns
fDeclare (DeclNode lhs _ _) = fDeclare lhs 
fDeclare (StructDefNode id _ _ _) = "local " ++ generateLhs id ++ ";\n"
fDeclare (SumTypeNode ds _) = intercalate "" $ map fDeclare ds
fDeclare (DeStructure ds _) = intercalate "" $ map fDeclare ds
fDeclare a = "> " ++ show a ++ "\n\n"

--- Generate Application Code
handlebinOp l "and" r pos = "((" ++ generate l ++ ")" ++ ":anded(SltThunk.create(function() return " ++ generate r ++ " end)))"
handlebinOp l "or" r pos = "((" ++ generate l  ++ ")" ++ ":ored(SltThunk.create(function() return " ++ generate r ++ " end)))"
handlebinOp l "=" r pos = "((" ++ generate l ++ ")" ++ ":eq(" ++ generate r ++ "))"
handlebinOp l "/=" r pos = "((" ++ generate l ++ ")" ++ ": neq(" ++ generate r ++ "))"
handlebinOp l ">" r pos = "((" ++ generate l ++ ")" ++ ":gt(" ++ generate r ++ "))"
handlebinOp l ">=" r pos = "((" ++ generate l ++ ")" ++ ":gte(" ++ generate r ++ "))"
handlebinOp l "<" r pos = "((" ++ generate l ++ ")" ++ ":lt(" ++ generate r ++ "))"
handlebinOp l "<=" r pos = "((" ++ generate l ++ ")" ++ ":lte(" ++ generate r ++ "))"
handlebinOp l ".." r pos = "((" ++ generate l ++ ")" ++ ":concat(SltThunk.create(function() return " ++ generate r ++ " end)))"
handlebinOp l "@" r pos = "((" ++ generate l ++ ")" ++ ":isType(" ++ generate r ++ "))"
handlebinOp l "." r pos = "(((" ++ generate l ++ ")" ++ ":getProperty(" ++ generate r ++ "))())"
handlebinOp l op r pos = "(" ++ generate l ++ ")" ++ op ++ "(" ++ generate r ++ ")"  

handleUnaryOp "-" e = "(" ++ generate e ++ ":neg())"
handleUnaryOp "not" e = "(" ++ generate e ++ ":notted())"

generateLhs (TupleNode t _) = intercalate ", " (map generateLhs t)
generateLhs (DeStructure ds _) = intercalate ", " (map generateLhs ds)
generateLhs (IdentifierNode id _) = id
generateLhs (DataNode id _) = id


luaPos (P.SourcePos s ln cn) = 
    "{\"" ++ s ++ "\", " ++ tail (dropWhile (/= ' ') (show ln)) ++ ", " ++ tail(dropWhile (/= ' ') (show cn)) ++ "}"

generate (StringNode str pos) = "(SltString.create(\"" ++ str ++ "\", " ++ luaPos pos ++"))"
generate (NumNode n pos) = "(SltNum.create(" ++ n ++ ", " ++ luaPos pos ++ "))"
generate (IdentifierNode id _) = id ++ "()"
generate (BinOpNode l op r pos) = handlebinOp l op r pos
generate (IfNode ce te ee _) = 
    "(" ++ generate ce ++ ":is_true()" ++ " and " ++ generate te ++ gee ++ ")" where
        gee =
            case ee of
                Just a -> " or " ++ generate a
                Nothing -> ""
generate (ProgramNode ds _) = intercalate "\n" (map generate ds)
generate (TupleNode t pos) = 
    "(SltTuple.create(" ++ "{" ++ intercalate ", " (
        map (\e -> "SltThunk.create(function() return " ++ generate e ++ "end)") t
        ) ++ "}" ++ ", " ++ luaPos pos ++ "))"

generate lst@(ListNode xs pos) = "(SltList.fromValues(" ++ "{" ++ intercalate ", " (map generate xs) ++ "}" ++ ", " ++ luaPos pos ++ "))"

generate (CallNode f args _) = 
    generate f ++ intercalate "" (map (\arg -> "(SltThunk.create(function() return " ++ generate arg ++ " end))") args)
generate (DeclNode lhs rhs pos) = generateLhs lhs ++ " = " ++ evalRhs where
    evalRhs =
        case lhs of
            TupleNode ts _ -> "SltValue.unwrap(" ++ generate rhs ++ ", " ++ show (length ts) ++ ")"
            DeStructure ds _ -> "SltValue.destructure(" ++ generate rhs ++ ", " ++ show (length ds) ++ ")"
            IdentifierNode{} -> "SltThunk.create(function() return " ++ generate rhs ++ " end)"
generate (FuncDefNode _ pure args expr pos) = 
    fun
    where
        fun = gen ++ " " ++ generate expr ++ unwords (map (const ("end, " ++ luaPos pos ++ ", " ++ turnBool pure ++ ")")) args)
        gen = intercalate "" (map (\arg -> "SltFunc.create(function (" ++ generateLhs arg ++") return ") args)
        turnBool b = if b then "true" else "false"
generate (BoolNode b pos) = "SltBool.create(" ++ b ++ ", " ++ luaPos pos ++ ")"
generate (SequenceIfNode fs pos) = 
    (intercalate " or " (map generate fs)) ++ 
        " or error(SltError.create(\"CaseError\", \"None of the cases matched\", {loc = " ++ luaPos pos ++ "}))"
generate (UnaryExpr op e _) = handleUnaryOp op e
generate (DataNode n _) = "\"" ++ n ++ "\""
generate (SumTypeNode ds _) = intercalate "\n" (map generate ds)
generate (WhereNode exp ds _) = 
    "SltFunc.create(function() \n" ++ fDecls ++ intercalate ";\n" (map generate ds) ++ "\nreturn " ++ generate exp ++ " end)()" where
        fDecls = intercalate ";\n" (map fDeclare ds)
generate strct@(StructDefNode id table ov pos) = 
    generateLhs id ++ " = " ++ struct
    where 
        ident = generateLhs id
        argTable = "{" ++ intercalate "; " (map ((\a -> a ++ " = " ++ "\"\"") . generateLhs) table) ++ "}"
        inheritance = 
            case ov of
                Just a -> "\"" ++ generateLhs a ++ "\", "
                Nothing -> "nil, "
        struct =
            "SltFunc.create(function(tb, loc)" ++ " return " ++
            "SltStruct.create(\"" ++ ident ++ "\", " ++ inheritance ++ argTable ++ ", tb, loc) end, " ++ luaPos pos ++")"

generate (StructInstanceNode id ls pos) = 
    generateLhs id ++ "({" ++ intercalate "; " (map generate ls) ++ "}, " ++ luaPos pos ++ ")"

runGenerator :: Either String Node -> String 
runGenerator (Left e) = e
runGenerator (Right e) = fDeclare e ++ "\n" ++ generate e
