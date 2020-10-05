module Parser where

import Control.Applicative as Applicative
import Control.Monad
import Data.Char
import Data.Text (Text)
import Data.List
import Data.Void
import EitherUtility
import Text.Megaparsec as P
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

import Debug.Trace

lower = oneOf "abcdefghijklmnopqrstuvwxyz" :: Parser Char
upper = oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ" :: Parser Char
digit = oneOf "1234567890" :: Parser Char
newline = oneOf "\n;" :: Parser Char
newlines = P.many Parser.newline
space = char ' '
spaces = P.many Parser.space
mspaces = 
    do
        Parser.space
        Parser.spaces
keyword k = Text.Megaparsec.Char.string (showL k) :: Parser String

notKeyword = notFollowedBy (choice keywords) where
    keywords = map ((\a -> Text.Megaparsec.Char.string a :: Parser String) . showL) [
            Parser.If,
            Parser.Then,
            Parser.Else,
            Parser.True,
            Parser.False,
            Parser.Not,
            Parser.Type,
            Parser.Class,
            Parser.Every,
            Parser.Is,
            Parser.Where,
            Parser.End,
            Parser.Or,
            Parser.And,
            Parser.Open,
            Parser.Include
        ]

showL k = map toLower (show k)

data Keyword =
    If
    | Then
    | Else
    | True
    | False
    | Not
    | Type
    | Class
    | Every
    | Is
    | End
    | Where
    | And
    | Or
    | Include
    | Open
    deriving(Show, Eq)

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
    | FuncDefNode (Maybe Node) Bool [Node] Node SourcePos
    | MethodNode Node [Node] SourcePos
    | NewMethodNode Node Node Node SourcePos
    | StructInstanceNode Node [Node] SourcePos
    | StructDefNode Node [Node] (Maybe Node) SourcePos
    | SumTypeNode [Node] SourcePos
    | DeStructure [Node] SourcePos
    | DataNode String SourcePos
    | BoolNode String SourcePos
    | WhereNode Node [Node] SourcePos
    | MultipleDefinitionNode [Node]
    deriving (Eq)

instance Show Node where
    show (StringNode str _) = "\"" ++ str ++ "\""
    show (SequenceIfNode ifs _) = "[" ++ intercalate ", " (map show ifs) ++ "]"
    show (NumNode n _) = n
    show (IdentifierNode id _) = id
    show (CallNode callee args _) = show callee ++ "(" ++ intercalate ", " (map show args) ++ ")"
    show (WhereNode e ds _) = show e ++ "{" ++ intercalate ", " (map show ds) ++"}"
    show (ProgramNode arr _) = intercalate "\n" (map show arr)
    show (ListNode xs _) = "[" ++ intercalate ", " (map show xs) ++ "]"
    show (TupleNode arr _) = "(" ++ intercalate ", " (map show arr) ++ ",)"
    show (BinOpNode a op b _) = "(" ++ show a ++ op ++ show b ++ ")"
    show (IfNode ce te ee _) = 
        "if " ++ show ce ++ " then " ++ show te ++ lp where
            lp = 
                case ee of
                    Just e -> " else " ++ show e
                    Nothing -> ""
    show (DeclNode lhs rhs _) = "let " ++ show lhs ++ " <- " ++ show rhs
    show (BoolNode b _) = b
    show (UnaryExpr u n _) = "(" ++ u ++ " " ++ show n ++ ")"
    show (DataNode n _) = n
    show (StructDefNode id ls ov _) = show id ++ "{" ++ intercalate "; " (map show ls) ++ "}" ++ decCase 
        where
            decCase =
                case ov of
                    Just a -> " -> " ++ show a
                    Nothing -> ""

    show (SumTypeNode ds _) = intercalate " | " (map show ds)
    show (StructInstanceNode id ls _) = show id ++ "{" ++ intercalate "; " (map show ls) ++ "}"
    show (DeStructure ds _) = "{" ++ intercalate ", " (map show ds) ++ "}"
    show (MethodNode id args _) = "open " ++ show id ++ "(" ++ intercalate ", " (map show args) ++ ")"
    show (NewMethodNode id ce te _) = 
        "close " ++ show id ++ " ? " ++ show ce ++ " -> " ++ show te
    show (FuncDefNode f _ args e _) = fname ++ " <- " ++ "(" ++ intercalate ", " (map show args) ++ ")" ++ " -> " ++ show e where
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
extractStructInstance (StructInstanceNode id ls _) = (id, ls)

type Parser = Parsec Void String

string :: Char -> Parser Node
string c =
    do
        pos <- getSourcePos
        str <- (char c *> manyTill L.charLiteral (char c)) :: Parser String
        return $ StringNode str pos

number :: Parser Node
number =
    do
        pos <- getSourcePos
        fs <- digit :: Parser Char
        str <- P.many digit :: Parser String
        return $ NumNode (fs : str) pos

fractional :: Parser Node
fractional =
    do
        pos <- getSourcePos
        dec <- number
        Text.Megaparsec.Char.string "." :: Parser String
        frac <- number
        return $ NumNode (extractString dec ++ "." ++ extractString frac) pos

identifier :: Parser Node
identifier =
    do
        pos <- getSourcePos
        notKeyword
        fc <- lower
        l <- P.many (lower <|> upper <|> digit <|> oneOf "_")
        return $ IdentifierNode (fc : l) pos

dataName :: Parser Node
dataName =
    do
        pos <- getSourcePos
        fc <- upper
        l <- P.many (lower <|> upper <|> digit)
        return $ DataNode (fc : l) pos

structInstanceExpr = 
    do
        pos <- getSourcePos
        id <- dataName
        try (
            do
                Text.Megaparsec.Char.string "{"
                ls <- seqInstance
                Text.Megaparsec.Char.string "}"
                return $ StructInstanceNode id ls pos
            ) <|> return (StructInstanceNode id [] pos)
    where
        seqInstance = commaSep seqPair
        seqPair =
            do
                id <- identifier
                spaces
                Text.Megaparsec.Char.string "::"
                spaces
                pos <- getSourcePos
                thing <- expr
                return $ DeclNode id thing pos

commaSep p  = p `sepBy` (try (spaces *> (Text.Megaparsec.Char.string ", ") <* spaces) :: Parser String)

list = 
    do
        pos <- getSourcePos
        Text.Megaparsec.Char.string "["
        spaces
        ls <- commaSep Parser.expr
        spaces
        Text.Megaparsec.Char.string "]"
        return $ ListNode ls pos

tuple =
    do
        pos <- getSourcePos
        Text.Megaparsec.Char.string "("
        spaces
        ls <- commaSep Parser.expr
        spaces
        Text.Megaparsec.Char.string ")"
        return $ TupleNode ls pos
    where
        commaSep p  = 
            try (p `endBy` (spaces *> (Text.Megaparsec.Char.string ",") <* spaces :: Parser String))
            <|> p `sepBy` (spaces *> (Text.Megaparsec.Char.string ",") <* spaces :: Parser String)

everyExpr = 
    do
        pos <- getSourcePos
        keyword Every
        mspaces
        ls <- expr
        mspaces
        keyword Is
        mspaces
        me <- expr
        sugar <- (do
            spaces
            keyword If
            spaces
            re <- expr
            return [
                    FuncDefNode Nothing Prelude.True [IdentifierNode "x" pos] me pos, 
                    FuncDefNode Nothing Prelude.True [IdentifierNode "x" pos] re pos, 
                    ls
                    ]
            ) <|> (
                do
                return [
                        FuncDefNode Nothing Prelude.True [IdentifierNode "x" pos] me pos, 
                        FuncDefNode Nothing Prelude.True [IdentifierNode "x" pos] (BoolNode "true" pos) pos, 
                        ls
                        ]
                )
        return $ CallNode (IdentifierNode "map_and_filter" pos) sugar pos

atom = choice [
    Parser.ifExpr,
    Parser.lambdaExpr,
    try Parser.parens,
    Parser.everyExpr,
    notExpr,
    negExpr,
    boolean,
    tuple,
    Parser.caseExpr,
    Parser.list,
    Parser.string '"', 
    try Parser.fractional, 
    Parser.number,
    Parser.structInstanceExpr,
    Parser.identifier
    ]

lambdaExpr =
    do
        pos <- getSourcePos
        Text.Megaparsec.Char.string "\\"
        spaces
        try (fullLamba pos) <|> basicLambda pos
    where
        fullLamba pos =
            do
                args <- logicalExpr `sepBy1` (Text.Megaparsec.Char.string "," <* spaces)
                spaces
                Text.Megaparsec.Char.string "->"
                spaces
                e <- logicalExpr
                return $ FuncDefNode Nothing Prelude.True args e pos
        
        basicLambda pos =
            do
                e <- logicalExpr
                return $ FuncDefNode Nothing Prelude.True [IdentifierNode "x" pos] e pos


boolean =
    do
        pos <- getSourcePos
        b <- keyword Parser.True <|> keyword Parser.False
        return $ BoolNode b pos

parens =
    do
        Text.Megaparsec.Char.string "("
        spaces
        e <- expr
        spaces
        char ')'
        return e

-- Slow, backtracks a lot, cluncky but elegant, and proven by time
-- binOp fa ops fb ret =
--     x <|> fa
--     where
--         x = try $ do
--                 pos <- getSourcePos
--                 a <- fa
--                 spaces
--                 op <- ops
--                 spaces
--                 b <- fb
--                 return $ ret a op b pos

-- New, not tested enogh, uglier, but faster by a lot 
binOp :: Parser Node -> Parser String ->  Parser Node -> (Node -> String -> Node -> SourcePos -> Node) -> Parser Node
binOp fa ops fb ret =
    do
        pos <- getSourcePos
        a <- fa
        try (
            do
                spaces
                op <- ops
                spaces
                b <- fb
                return $ ret a op b pos
            ) <|> return a

lhs =
    do
        id <- mainLhs
        spaces
        Text.Megaparsec.Char.string "<-"
        return id
    where
        mainLhs = try fDef <|> identifier <|> tuple <|> destructureExpr

        destructureExpr = 
            do
                pos <- getSourcePos
                ls <- Text.Megaparsec.Char.string "{" *> spaces *>
                        identifier `sepBy1` (Text.Megaparsec.Char.string "," <* spaces) 
                    <* spaces <* Text.Megaparsec.Char.string "}"
                return $ DeStructure ls pos
        fDef =
            do
                pos <- getSourcePos
                callee <- identifier
                spaces *> Text.Megaparsec.Char.string ":" <* spaces
                args <- identifier `sepBy1` (Text.Megaparsec.Char.string "," <* spaces)
                spaces
                return $ CallNode callee args pos

structDef =
    do
        pos <- getSourcePos
        keyword Type
        spaces
        id <- dataName
        spaces
        Text.Megaparsec.Char.string "<-"
        spaces
        fields id pos <|> structs id
    where
        structs overarch =
            do
                pos <- getSourcePos
                sts <- structInstanceExpr `sepBy1` 
                    try (spaces *> newlines *> spaces *> Text.Megaparsec.Char.string "|" <* spaces <* newlines <* spaces)
                let ls = zip (map extractMId sts) (map extractIds sts)
                let defs = map (\(id, xs) -> StructDefNode id xs (Just overarch) pos) ls
                let fdefs = map makeFun defs
                return $ MultipleDefinitionNode $ SumTypeNode defs pos : fdefs

        lowId (DataNode id pos) = IdentifierNode (map toLower id) pos
        lowId (IdentifierNode id pos) = IdentifierNode (map toLower id) pos
        makeFun (strct@(StructDefNode id xs _ pos)) = 
            DeclNode (lowId id) (FuncDefNode (Just $ lowId id) Prelude.True xs (instantiate xs strct) pos) pos

        instantiate rhss (StructDefNode id lhss _ pos) = StructInstanceNode id (zipWith (\a b -> DeclNode a b pos) rhss lhss) pos 

        extractIds strct@(StructInstanceNode _ ls _) = snd (extractStructInstance strct)

        extractMId strct@(StructInstanceNode id _ _) = id

        fields id pos =
            do
                ls <- Text.Megaparsec.Char.string "{" *> commaSep identifier <* Text.Megaparsec.Char.string "}"
                let stDef = StructDefNode id ls Nothing pos
                let fDef = makeFun stDef
                return $ MultipleDefinitionNode $ stDef : [fDef]

        structInstanceExpr = 
            do
                pos <- getSourcePos
                id <- dataName
                try (
                    do
                        Text.Megaparsec.Char.string "{"
                        ls <- seqInstance
                        Text.Megaparsec.Char.string "}"
                        return $ StructInstanceNode id ls pos
                    ) <|> return (StructInstanceNode id [] pos)
            where
                seqInstance = commaSep seqPair
                seqPair =
                    do
                        id <- identifier
                        pos <- getSourcePos
                        return $ IdentifierNode (extractString id) pos

decl =
    do
        pos <- getSourcePos
        id <- lhs
        new_id <- 
            case id of
                (CallNode c arg _) -> return c
                _ -> return id
        spaces
        e <- whereExpr
        new_e <- 
            case id of
                (CallNode c arg _) -> return $ FuncDefNode (Just c) Prelude.True arg e pos
                _ -> return e
        return $ DeclNode new_id new_e pos

includes =
    do
        pos <- getSourcePos
        P.many Parser.newline
        is <- include `sepBy` try (spaces *> newlines *> spaces *> lookAhead include <* spaces <* newlines <* spaces) 
        return $ ListNode is pos
    where
        include :: Parser Node
        include =
            do
                pos <- getSourcePos
                keyword Include
                mspaces
                Parser.string '"'

decls xs =
    do
        pos <- getSourcePos
        P.many Parser.newline
        a <- includes
        P.many Parser.newline
        spaces
        dcs <- 
            (try mewMethod <|> classStmnt <|> structDef <|> decl <|> methodDecl) 
                `endBy1` (spaces *> Parser.newline *> P.many Parser.newline <* spaces :: Parser String)
        return $ ProgramNode (concatLists dcs $ getLists xs) pos
    where
        getLists ns = map extractList ns
        concatLists dcs xs =
            case xs of
                [] -> dcs
                a -> foldr (++) (head xs) (tail xs) ++ dcs

whereExpr =
    do 
        rns <- expr
        try (
                do
                    pos <- getSourcePos
                    mspaces
                    keyword Where
                    mspaces
                    ds <- (newlines *> spaces *> decl) `sepBy1` notFollowedBy (spacificSpaces *> keyword End)
                    spacificSpaces *> keyword End
                    return $ WhereNode rns ds pos
            ) <|> return rns
    where
        spacificSpaces = (mspaces *> newlines *> spaces) <|> (Parser.newline *> newlines *> spaces)

expr =
    do 
        pos <- getSourcePos
        xs pos  
    where
        spacificSpaces = (mspaces *> newlines *> spaces) <|> (Parser.newline *> newlines *> spaces)
        xs pos = 
            do
                l <- backExpr `sepBy1` try (spaces *> Text.Megaparsec.Char.string "|>" <* spaces)
                return $ foldr (\a b -> CallNode a [b] pos) (head l) (reverse $ tail l)

backExpr =
    do
        pos <- getSourcePos
        bs pos 
    where
        bs pos = 
            do
                xs <- logicalExpr `sepBy1` try (spaces *> Text.Megaparsec.Char.string "<|" <* spaces)
                let (l:ls) = reverse xs
                return $ foldl (\a b -> CallNode b [a] pos) l ls

logicalExpr = binOp compExpr (Text.Megaparsec.Char.string "and" <|> Text.Megaparsec.Char.string "or") logicalExpr BinOpNode

compExpr = binOp typeExpr ops compExpr BinOpNode where
    ops =
        (
        Text.Megaparsec.Char.string "=" 
        <|> Text.Megaparsec.Char.string "/="
        <|> Text.Megaparsec.Char.string ">="
        <|> Text.Megaparsec.Char.string ">"
        <|> Text.Megaparsec.Char.string "<="
        <|> Text.Megaparsec.Char.string "<"
        ) :: Parser String

typeExpr = binOp arithExpr (Text.Megaparsec.Char.string "@") (dataName <|> typeExpr) BinOpNode

arithExpr = binOp term (Text.Megaparsec.Char.string "+" <|> Text.Megaparsec.Char.string "-") arithExpr BinOpNode

term = binOp Parser.concat (Text.Megaparsec.Char.string "*" <|> Text.Megaparsec.Char.string "/") term BinOpNode

concat = binOp infixOp (Text.Megaparsec.Char.string "..") expr BinOpNode

infixOp = 
    binOp application op infixOp (\a op b pos -> CallNode (IdentifierNode op pos) [a, b] pos)
    where
        op = do extractString <$> identifier

application =
    do
        pos <- getSourcePos
        callee <- index
        try (
            do
                m <- mid
                let args = arr m where
                    arr(ListNode arr _) = arr
                return $ CallNode callee args pos
            ) <|> return callee
        where
            mid = 
                do
                    pos <- getSourcePos
                    spaces *> Text.Megaparsec.Char.string ":" <* spaces
                    args <- logicalExpr `sepEndBy1` (Text.Megaparsec.Char.string "," <* spaces)
                    return $ ListNode args pos

index = 
    do
        pos <- getSourcePos
        og <- access
        ls <- P.many tIndex
        return $ case last $ Just og : ls of 
            Just _ -> folded og ls pos
            Nothing -> FuncDefNode Nothing Prelude.True [IdentifierNode "elwegot" pos] (folded og ls pos) pos
    where
        folded og ls pos = foldr (makeCall pos) og (reverse ls)

        makeCall pos fa b = case fa of
            Just a -> CallNode (IdentifierNode "access" pos) [a, b] pos
            Nothing -> CallNode (IdentifierNode "access" pos) [b, IdentifierNode "elwegot" pos] pos
        tIndex =
            try (do
                    Text.Megaparsec.Char.string "["
                    e <- expr
                    Text.Megaparsec.Char.string "]"
                    return $ Just e)
            <|> 
            do
                Text.Megaparsec.Char.string "["
                Text.Megaparsec.Char.string "]"
                return Nothing

access =  
    do
        pos <- getSourcePos
        l <- atom `sepBy1` try (spaces *> Text.Megaparsec.Char.string "." <* notFollowedBy (Text.Megaparsec.Char.string ".") <* spaces)
        let mpl = map makeBin (tail l)
        return $ foldl' (\a b -> BinOpNode a "." b pos) (head l) mpl
    where
        makeBin :: Node -> Node
        makeBin n@(BinOpNode a op b pos) = BinOpNode a op (makeBin b) pos
        makeBin n@(IdentifierNode s pos) = StringNode s pos
        makeBin n = n

methodDecl = 
    do
        pos <- getSourcePos
        keyword Open
        mspaces
        id <- identifier
        spaces *> Text.Megaparsec.Char.string ":" <* spaces
        args <- identifier `sepBy1` (Text.Megaparsec.Char.string "," <* spaces)
        return $ MethodNode id args pos

mewMethod =
    do
        pos <- getSourcePos
        id <- identifier
        spaces *> Text.Megaparsec.Char.string "?" <* spaces
        cond <- expr
        spaces
        Text.Megaparsec.Char.string "->"
        spaces
        exp <- expr
        return $ NewMethodNode id cond exp pos

classStmnt =
    do
        pos <- getSourcePos
        keyword Class
        mspaces
        id <- identifier
        spaces *> Text.Megaparsec.Char.string ":" <* spaces
        args <- expr `sepBy1` (Text.Megaparsec.Char.string "," <* spaces)
        mnewlines
        seqPos <- getSourcePos
        allCases <- cases `sepBy1` 
            notFollowedBy (try 
                (P.many Parser.newline *> spaces *> (
                    eof *> (StringNode "" <$> getSourcePos)
                    <|> keyword Class *> (StringNode "" <$> getSourcePos)
                    <|> try decl 
                    <|> structDef
                    <|> mewMethod)))
        return $ DeclNode id (FuncDefNode (Just id) Prelude.True args (SequenceIfNode allCases seqPos) seqPos) pos
    where
        cases = do
            newlines
            spaces
            pos <- getSourcePos
            spaces
            cond <- expr
            spaces
            Text.Megaparsec.Char.string "->"
            spaces
            thenExpr <- expr
            return $ IfNode cond thenExpr Nothing pos
        mnewlines = Parser.newline *> newlines

notExpr =
    do
        pos <- getSourcePos
        keyword Not
        Parser.spaces
        expr <- compExpr
        return $ UnaryExpr "not" expr pos

negExpr =
    do
        pos <- getSourcePos
        Text.Megaparsec.Char.string "-"
        Parser.spaces
        e <- expr
        return $ UnaryExpr "-" e pos

ifExpr =
    do
        pos <- getSourcePos
        keyword If
        mspaces
        c <- expr
        mspaces
        keyword Then
        mspaces
        te <- expr
        mspaces
        keyword Else
        mspaces
        ee <- expr
        return $ IfNode c te (Just ee) pos

caseExpr =
    do
        pos <- getSourcePos
        Text.Megaparsec.Char.string "|"
        spaces
        fls <- p `sepBy` (spaces *> (Text.Megaparsec.Char.string "|") <* spaces :: Parser String)
        return $ SequenceIfNode fls pos 
    where 
        p = 
            do
                pos <- getSourcePos                
                ce <- expr
                spaces
                Text.Megaparsec.Char.string "->"
                spaces
                te <- expr
                return $ IfNode ce te Nothing pos

parse xs = decls xs <* eof
