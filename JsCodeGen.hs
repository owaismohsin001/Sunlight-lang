module JsCodeGen where

import Nodes
import Modes
import qualified Text.Megaparsec as P
import Data.Maybe
import Data.List
import Debug.Trace


-- Generate Forward declarations
fDeclare (IdentifierNode id _) = "let " ++ id ++ ";\n"
fDeclare (DataNode id _) = "let " ++ id ++ ";\n"
fDeclare (TupleNode t _) = intercalate "" $ map fDeclare t
fDeclare (ProgramNode ns _) = intercalate "" $ map fDeclare ns
fDeclare (DeclNode lhs _ _) = fDeclare lhs 
fDeclare (StructDefNode id _ _ _ _) = "let " ++ generateLhs id ++ ";\n"
fDeclare (SumTypeNode ds _) = intercalate "" $ map fDeclare ds
fDeclare (DeStructure ds _) = intercalate "" $ map fDeclare ds
fDeclare (ExternalNode (StringNode id _) ns _) = "import {" ++ intercalate ", " (map generateLhs ns) ++ "} from \"./libs/" ++ id ++ ".js\";\n"
fDeclare a = error $ show a ++ "\n\n"

--- Generate Application Code
handlebinOp l "&" r pos = "((" ++ generate l ++ ")" ++ ".anded(new SltThunk(() => " ++ generate r ++ ")))"
handlebinOp l "|" r pos = "((" ++ generate l  ++ ")" ++ ".ored(new SltThunk(() => " ++ generate r ++ ")))"
handlebinOp l "=" r pos = "((" ++ generate l ++ ")" ++ ".eq(" ++ generate r ++ "))"
handlebinOp l "/=" r pos = "((" ++ generate l ++ ")" ++ ".neq(" ++ generate r ++ "))"
handlebinOp l ">" r pos = "((" ++ generate l ++ ")" ++ ".gt(" ++ generate r ++ "))"
handlebinOp l ">=" r pos = "((" ++ generate l ++ ")" ++ ".gte(" ++ generate r ++ "))"
handlebinOp l "<" r pos = "((" ++ generate l ++ ")" ++ ".lt(" ++ generate r ++ "))"
handlebinOp l "<=" r pos = "((" ++ generate l ++ ")" ++ ".lte(" ++ generate r ++ "))"
handlebinOp l ".." r pos = "((" ++ generate l ++ ")" ++ ".concat(new SltThunk(() => " ++ generate r ++ ")))"
handlebinOp l "@" r pos = "((" ++ generate l ++ ")" ++ ".isType(" ++ generate r ++ "))"
handlebinOp l "." r pos = "(((" ++ generate l ++ ")" ++ ".getProperty(" ++ generate r ++ "))())"
handlebinOp l "+" r pos = "((" ++ generate l ++ ")" ++ ".add(" ++ generate r ++ "))"
handlebinOp l "-" r pos = "((" ++ generate l ++ ")" ++ ".sub(" ++ generate r ++ "))"
handlebinOp l "*" r pos = "((" ++ generate l ++ ")" ++ ".mul(" ++ generate r ++ "))"
handlebinOp l "/" r pos = "((" ++ generate l ++ ")" ++ ".div(" ++ generate r ++ "))"
handlebinOp l op r pos = 
    if op `elem` ["+", "-", "*", "/"] then "((" ++ generate l ++ ")" ++ op ++ "(" ++ generate r ++ "))"
    else error $ "No operator " ++ op ++ " defined"


handleUnaryOp "-" e = "(" ++ generate e ++ ".neg())"

generateLhs (TupleNode t _) = "[" ++ intercalate ", " (map generateLhs t) ++ "]"
generateLhs (DeStructure ds _) = "[" ++ intercalate ", " (map generateLhs ds) ++ "]"
generateLhs (IdentifierNode id _) = id
generateLhs (DataNode id _) = id


luaPos (P.SourcePos s ln cn) = 
    "[" ++ tail (dropWhile (/= ' ') (show ln)) ++ ", " ++ tail(dropWhile (/= ' ') (show cn)) ++ ", \"" ++ s ++ "\"]" 

generate (StringNode str pos) = "(new SltString(" ++ show str ++ ", " ++ luaPos pos ++"))"
generate (NumNode n pos) = "(new SltNum(" ++ n ++ ", " ++ luaPos pos ++ "))"
generate (IdentifierNode id _) = id ++ "()"
generate (TypeRefNode dt pos) = "new SltType(" ++ generate dt ++ ", " ++ luaPos pos ++ ")"
generate (BinOpNode l op r pos) = handlebinOp l op r pos
generate (IfNode ce te ee _) = 
    generate ce ++ ".is_true()" ++ " ? " ++ generate te ++ maybe "" (\a -> " : " ++ generate a) ee
generate (ProgramNode ds _) = intercalate ";\n" (map generate ds)
generate (TupleNode t pos) = 
    "(new SltTuple(" ++ "[" ++ intercalate ", " (
        map (\e -> "new SltThunk(() => " ++ generate e ++ ")") t
        ) ++ "]" ++ ", " ++ luaPos pos ++ "))"

generate lst@(ListNode xs pos) = "(SltList.fromValues(" ++ "[" ++ intercalate ", " (map generate xs) ++ "]" ++ ", " ++ luaPos pos ++ "))"

generate (CallNode f args _) = 
    generate f ++ intercalate "" (map (\arg -> "(new SltThunk(() => " ++ generate arg ++ "))") args)
generate (DeclNode lhs rhs pos) = generateLhs lhs ++ " = " ++ evalRhs where
    evalRhs =
        case lhs of
            TupleNode ts _ -> "unwrap(" ++ generate rhs ++ ", " ++ show (length ts) ++ ")"
            DeStructure ds _ -> "destructure([" ++ generate rhs ++ ", " ++ show (length ds) ++ ")"
            IdentifierNode o _ -> "(new SltThunk(() => " ++ generate rhs ++ "))"
generate (FuncDefNode _ args expr bh pos) = 
    "(" ++ fun ++ ")"
    where
        fun = gen ++ " " ++ generate expr ++ unwords (map (const (", " ++ luaPos pos ++ ", " ++ turnBool bh ++ ")")) args)
        gen = intercalate "" (map (\arg -> "new SltFunc((" ++ generateLhs arg ++") => ") args)
        turnBool b = if b then "true" else "false"
generate (BoolNode b pos) = "new SltBool(" ++ b ++ ", " ++ luaPos pos ++ ")"
generate (SequenceIfNode fs pos) = 
    case fs of
        [] -> "error(\"CaseError\", \"None of the cases matched\", " ++ luaPos pos ++ ")"
        _ -> intercalate " : " (map generate fs) ++ 
            " : true ? error(\"CaseError\", \"None of the cases matched\", " ++ luaPos pos ++ ") : null"
generate (UnaryExpr op e _) = handleUnaryOp op e
generate (DataNode n _) = "\"" ++ n ++ "\""
generate (SumTypeNode ds _) = intercalate "\n" (map generate ds)
generate (WhereNode exp ds _) = 
    "((() => {\n" ++ fDecls ++ intercalate ";\n" (map generate ds) ++ "\nreturn " ++ generate exp ++ " })())" where
        fDecls = intercalate "\n" (map fDeclare ds)
generate strct@(StructDefNode id table b ov pos) = 
    generateLhs id ++ " = " ++ struct
    where 
        ident = generateLhs id
        inheritance = 
            case ov of
                Just a -> "\"" ++ generateLhs a ++ "\", "
                Nothing -> "null, "
        struct =
            "(tb, loc)" ++ " => " ++
            "new SltStruct(\"" ++ ident ++ "\", " ++ inheritance ++ makeBool b ++", tb, loc)"
        makeBool b = if b then "true" else "false" 

generate (StructInstanceNode id ls _ pos) = 
    "(" ++ generateLhs id ++ "({" ++ intercalate ", " (map generateThis ls) ++ "}, " ++ luaPos pos ++ "))" where
        generateThis (DeclNode lhs rhs _) = generateLhs lhs ++ ": new SltThunk(() => " ++ generate rhs ++ ")"
generate (ExternalNode id _ _) = ""

runGenerator :: Either String Node -> String 
runGenerator (Left e) = e
runGenerator (Right e) = fDeclare e ++ "\n" ++ generate e

data Js = Deno

instance CompileMode Js where
    importGen _ = "import {SltValue, SltBool, SltNum, SltFunc, SltString, SltStruct, SltList, SltThunk, SltTuple, SltType, destructure, unwrap, listHead, listTail, baseStringify, evaluate, typeOf, ovTypeOf, unsafeWrite, error} from './SltRuntime.ts'\n;"
    codeGen _ n = runGenerator $ Right n
    fileNameGen _ = (++ ".js")
    startGen _ = ""
    sepGen _ = ";\n"
    endGen _ outName = ""++ outName ++"().toString()"
    invokeUtility Deno = "deno run "
