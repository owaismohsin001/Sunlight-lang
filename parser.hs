module Parser where

import Control.Applicative as Applicative
import Control.Monad
import Data.Char
import Nodes
import Data.Text (Text)
import Data.List
import Data.Void
import EitherUtility
import Text.Megaparsec as P
import Text.Megaparsec.Char
import MergeDefs
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
            Parser.Include,
            Parser.Mod,
            Parser.Lib
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
    | Mod
    | Lib
    deriving(Show, Eq)

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

identifier :: Bool -> Parser Node
identifier sQuote =
    do
        pos <- getSourcePos
        notKeyword
        a <- Text.Megaparsec.Char.string "$" <|> Text.Megaparsec.Char.string ""
        fc <- lower
        l <- if sQuote then P.many allowedPs <* char '\'' else P.many allowedPs
        return $ IdentifierNode ((a ++ [fc]) ++ l) pos
    <|> nsAccess
    where

        allowedPs = lower <|> upper <|> digit <|> undersore

        undersore = (
            ((char '_') :: Parser Char) 
                <* notFollowedBy ((char '_') :: Parser Char)
            ) :: Parser Char
        nsAccess =
            do
                t1 <- dataName
                termSuffix t1       
        termSuffix t1 = try $ unTrySuffix t1
        unTrySuffix t1 =
            do
                s <- singleSuffix t1
                loop s
        singleSuffix t1 =
            do
                pos <- getSourcePos
                op <- Text.Megaparsec.Char.string "::"
                t2 <- dataName <|> identifier Prelude.False
                loop $ IdentifierNode (extractString t1 ++ "__" ++ extractString t2) pos
        loop t = (unTrySuffix t <|> return t) :: Parser Node

dataName :: Parser Node
dataName =
    try nsDataAccess <|> dataNameFormula
    where
        dataNameFormula =
            do
                pos <- getSourcePos
                a <- Text.Megaparsec.Char.string "$" <|> Text.Megaparsec.Char.string ""
                fc <- upper
                l <- P.many (lower <|> upper <|> digit)
                return $ DataNode ((a ++ [fc]) ++ l) pos
        nsDataAccess =
            do
                t1 <- dataNameFormula
                termSuffix Nothing t1      
            where 
                termSuffix stp t1 = try $ unTrySuffix stp t1
                unTrySuffix stp t1 =
                    do
                        s <- singleSuffix stp t1
                        loop stp s
                singleSuffix stp t1 =
                    do
                        pos <- getSourcePos
                        op <- Text.Megaparsec.Char.string "::"
                        t2 <- dataNameFormula
                        x stp t1 t2 pos

                loop :: Maybe Node -> Node -> Parser Node
                loop stp t = case stp of
                    Just a -> return a
                    Nothing -> unTrySuffix Nothing t <|> return t :: Parser Node

                x stp t1 t2 pos =
                    (
                        lookAhead (Text.Megaparsec.Char.string "::" :: Parser String) *> 
                            loop stp v
                    ) <|> loop (Just v) v where
                        v = DataNode (extractString t1 ++ "__" ++ extractString t2) pos

structInstanceExpr = 
    do
        pos <- getSourcePos
        id <- dataName
        try (
            do
                Text.Megaparsec.Char.string "{"
                ls <- seqInstance
                Text.Megaparsec.Char.string "}"
                return $ StructInstanceNode id ls Prelude.False pos
            ) <|> return (StructInstanceNode id [] Prelude.False pos)
    where
        seqInstance = commaSep seqPair
        seqPair =
            do
                id <- identifier Prelude.False
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
                    FuncDefNode Nothing [IdentifierNode "x" pos] me pos, 
                    FuncDefNode Nothing [IdentifierNode "x" pos] re pos, 
                    ls
                    ]
            ) <|> (
                do
                return [
                        FuncDefNode Nothing [IdentifierNode "x" pos] me pos, 
                        FuncDefNode Nothing [IdentifierNode "x" pos] (BoolNode "true" pos) pos, 
                        ls
                        ]
                )
        return $ CallNode (IdentifierNode "map_and_filter" pos) sugar pos

atom = choice [
    Parser.ifExpr,
    Parser.lambdaExpr,
    try Parser.parens,
    Parser.everyExpr,
    Parser.notExpr,
    Parser.negExpr,
    Parser.boolean,
    Parser.tuple,
    Parser.caseExpr,
    Parser.list,
    Parser.string '"', 
    try Parser.fractional, 
    Parser.number,
    try $ Parser.structInstanceExpr <* notFollowedBy (Text.Megaparsec.Char.string "::"),
    Parser.identifier Prelude.False
    ]

typeRef =
    do
        pos <- getSourcePos
        spaces
        Text.Megaparsec.Char.string "&"
        spaces
        e <- dataName
        spaces
        return $ TypeRefNode e pos

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
                return $ FuncDefNode Nothing args e pos
        
        basicLambda pos =
            do
                e <- logicalExpr
                return $ FuncDefNode Nothing [IdentifierNode "x" pos] e pos


boolean =
    do
        pos <- getSourcePos
        b <- keyword Parser.True <|> keyword Parser.False
        return $ BoolNode b pos

parens =
    do
        Text.Megaparsec.Char.string "("
        spaces
        e <- whereExpr
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
rBinOp :: Parser Node -> Parser String ->  Parser Node -> (Node -> String -> Node -> SourcePos -> Node) -> Parser Node
rBinOp fa ops fb ret =
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

binOp f ops ret = do
  t1 <- f
  loop t1
  where termSuffix t1 = try (do
          pos <- getSourcePos
          spaces
          op <- ops
          spaces
          t2 <- f
          loop (ret t1 op t2 pos))
        loop t = termSuffix t <|> return t

lhs =
    do
        id <- mainLhs
        spaces
        Text.Megaparsec.Char.string "<-"
        return id
    where
        mainLhs = try fDef <|> identifier Prelude.False <|> tuple <|> destructureExpr

        destructureExpr = 
            do
                pos <- getSourcePos
                ls <- Text.Megaparsec.Char.string "{" *> spaces *>
                        identifier Prelude.False `sepBy1` (Text.Megaparsec.Char.string "," <* spaces) 
                    <* spaces <* Text.Megaparsec.Char.string "}"
                return $ DeStructure ls pos

        fDef =
            do
                pos <- getSourcePos
                callee <- identifier Prelude.False
                spaces *> Text.Megaparsec.Char.string ":" <* spaces
                args <- identifier Prelude.False `sepBy1` (Text.Megaparsec.Char.string "," <* spaces)
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
        try (structs id) <|> fields id pos
    where
        structs overarch =
            do
                pos <- getSourcePos
                sts <- structInstanceExpr `sepBy1` 
                    try (spaces *> newlines *> spaces *> Text.Megaparsec.Char.string "|" <* spaces <* newlines <* spaces)
                let ls = zip3 (map extractMId sts) (map extractIds sts) (map extractStrict sts)
                let defs = map (\(id, xs, stct) -> StructDefNode id xs stct (Just overarch) pos) ls
                let fdefs = map makeFun defs
                return $ MultipleDefinitionNode $ SumTypeNode defs pos : fdefs

        lowId (DataNode id pos) = IdentifierNode (toLower (head id) : tail id) pos
        lowId (IdentifierNode id pos) = IdentifierNode (map toLower id) pos
        makeFun (strct@(StructDefNode id xs _ _ pos)) = 
            if xs == [] then 
                FromStruct $ DeclNode (lowId id) (instantiate xs strct) pos
            else
                FromStruct $ DeclNode (lowId id) (FuncDefNode (Just $ lowId id) xs (instantiate xs strct) pos) pos

        instantiate rhss (StructDefNode id lhss b _ pos) = StructInstanceNode id (zipWith (\a b -> DeclNode a b pos) rhss lhss) b pos 

        extractIds strct@(StructInstanceNode _ ls _ _) = snd (extractStructInstance strct)

        extractMId strct@(StructInstanceNode id _ _ _) = id

        extractStrict strct@(StructInstanceNode _ _ stct _) = stct

        fields id pos =
            do
                a <- (Text.Megaparsec.Char.string "!" <* spaces) <|> Text.Megaparsec.Char.string ""
                ls <- Text.Megaparsec.Char.string "{" *> commaSep (identifier Prelude.False) <* Text.Megaparsec.Char.string "}"
                let stDef = StructDefNode id ls (a /= "") Nothing pos
                let fDef = makeFun stDef
                return $ MultipleDefinitionNode $ stDef : [fDef]

        structInstanceExpr = 
            se <|> e where 
                se =
                    do
                        pos <- getSourcePos
                        Text.Megaparsec.Char.string "!" <* spaces 
                        ins <- structInstanceExpr
                        return $ StructInstanceNode (extractMId ins) (extractIds ins) Prelude.True pos
                e =
                    do
                        pos <- getSourcePos
                        id <- dataName
                        try (
                            do
                                Text.Megaparsec.Char.string "{"
                                ls <- seqInstance
                                Text.Megaparsec.Char.string "}"
                                return $ StructInstanceNode id ls Prelude.False pos
                            ) <|> return (StructInstanceNode id [] Prelude.False pos)
                seqInstance = commaSep seqPair
                seqPair =
                    do
                        id <- identifier Prelude.False
                        pos <- getSourcePos
                        return $ IdentifierNode (extractString id) pos

decl =
    modStmnt <|>
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
                    (CallNode c arg _) -> return $ FuncDefNode (Just c) arg e pos
                    _ -> return e
            return $ DeclNode new_id new_e pos

includes iType =
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
                keyword iType
                mspaces
                Parser.string '"'

modStmnt =
    do
        pos <- getSourcePos
        mname <- keyword Mod *> mspaces *> dataName <* newlines <* spaces
        ds <- (newlines *> spaces *> ((try mewMethod <|> classStmnt <|> structDef <|> decl <|> methodDecl) )) 
            `sepBy1` notFollowedBy (newlines *> spaces *> newlines *> keyword End)
        newlines *> spaces *> newlines *> keyword End
        let tds = map (differLhs mname) ds
        let flist = map (getDollar $ extractString mname) tds
        return $ MultipleDefinitionNode flist
    where
        differLhs :: Node -> Node -> Node
        differLhs mn (IdentifierNode id pos) = IdentifierNode (extractString mn ++ "__" ++ id) pos
        differLhs mn (DataNode id pos) = DataNode (extractString mn ++ "__" ++ id) pos
        differLhs mn (TupleNode ts pos) = TupleNode (map (differLhs mn) ts) pos
        differLhs mn (DeclNode lhs rhs pos) = DeclNode (differLhs mn lhs) (changeFun mn rhs) pos
        differLhs mn (StructDefNode id x st (Just o) pos) = StructDefNode (differLhs mn id) x st (Just $ differLhs mn o) pos
        differLhs mn (StructDefNode id x stct Nothing pos) = StructDefNode (differLhs mn id) x stct Nothing pos
        differLhs mn (DeStructure ids pos) = DeStructure (map (differLhs mn) ids) pos
        differLhs mn (SumTypeNode ds pos) = SumTypeNode (map (differLhs mn) ds) pos
        differLhs mn (MethodNode id args pos) = MethodNode (differLhs mn id) args pos
        differLhs mn (MultipleDefinitionNode ds) = MultipleDefinitionNode $ map (differLhs mn) ds
        differLhs mn (FromStruct (DeclNode lhs (FuncDefNode (Just id) args (StructInstanceNode sid sargs b spos) pos) dpos)) =
            FromStruct $ 
                DeclNode (differLhs mn lhs) 
                (FuncDefNode (Just $ differLhs mn id) args (StructInstanceNode (differLhs mn sid) sargs b spos) pos) 
                dpos
        differLhs mn (FromStruct (DeclNode lhs (StructInstanceNode sid sargs b spos) pos)) =
            FromStruct $ 
                DeclNode (differLhs mn lhs) 
                (StructInstanceNode (differLhs mn sid) sargs b spos)
                pos
        differLhs mn nm@NewMethodNode{} = nm
        differLhs _ a = error(show a ++ "\n")

        changeFun mn (FuncDefNode (Just id) args e pos) = FuncDefNode (Just $ differLhs mn id) args e pos
        changeFun mn n = n

decls xs =
    do
        pos <- getSourcePos
        P.many Parser.newline
        a <- includes Lib
        P.many Parser.newline
        b <- includes Mod
        P.many Parser.newline
        spaces
        dcs <- 
             pref *>
                (try mewMethod <|> classStmnt <|> structDef <|> decl <|> methodDecl) 
                `endBy` (spaces *> Parser.newline *> P.many Parser.newline <* spaces :: Parser String)
        return $ ProgramNode (concatLists dcs $ getLists xs) pos
    where
        pref = 
            P.many (Text.Megaparsec.Char.string "--" *> manyTill L.charLiteral (char '\n' :: Parser Char) *> P.many (Text.Megaparsec.Char.string "\n")) 
                <|> ((\a -> [[a]]) <$> Text.Megaparsec.Char.string "")
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
                    spaces
                    ds <- (newlines *> spaces *> decl) `sepBy1` notFollowedBy (spacificSpaces *> keyword End)
                    spacificSpaces *> keyword End
                    return $ WhereNode rns ds pos
            ) <|> return rns
    where
        spacificSpaces = (spaces *> newlines *> spaces) <|> (Parser.newline *> newlines *> spaces)

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

logicalExpr = binOp compExpr (Text.Megaparsec.Char.string "and" <|> Text.Megaparsec.Char.string "or") BinOpNode

compExpr = binOp typeExpr ops BinOpNode where
    ops =
        (
        Text.Megaparsec.Char.string "=" 
        <|> Text.Megaparsec.Char.string "/="
        <|> Text.Megaparsec.Char.string ">="
        <|> Text.Megaparsec.Char.string ">"
        <|> Text.Megaparsec.Char.string "<="
        <|> Text.Megaparsec.Char.string "<"
        ) :: Parser String

typeExpr = rBinOp arithExpr (Text.Megaparsec.Char.string "@") (dataName <|> arithExpr) BinOpNode

arithExpr = binOp term (Text.Megaparsec.Char.string "+" <|> Text.Megaparsec.Char.string "-") BinOpNode

term = binOp Parser.concat (Text.Megaparsec.Char.string "*" <|> Text.Megaparsec.Char.string "/") BinOpNode

concat = binOp infixOp (Text.Megaparsec.Char.string "..") BinOpNode

infixOp = rBinOp infixLOp op infixOp (\a op b pos -> CallNode (IdentifierNode op pos) [a, b] pos) where
    op = do extractString <$> identifier Prelude.False

infixLOp = binOp application op (\a op b pos -> CallNode (IdentifierNode op pos) [a, b] pos) where
    op = do extractString <$> identifier Prelude.True

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
                    args <- (typeRef <|> logicalExpr) `sepEndBy1` (Text.Megaparsec.Char.string "," <* spaces)
                    return $ ListNode args pos

index = 
    do
        pos <- getSourcePos
        og <- access
        ls <- P.many tIndex
        return $ case last $ Just og : ls of 
            Just _ -> folded og ls pos
            Nothing -> FuncDefNode Nothing [IdentifierNode "elwegot" pos] (folded og ls pos) pos
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
        id <- identifier Prelude.False
        spaces *> Text.Megaparsec.Char.string ":" <* spaces
        args <- identifier Prelude.False `sepBy1` (Text.Megaparsec.Char.string "," <* spaces)
        return $ MethodNode id args pos

mewMethod =
    do
        pos <- getSourcePos
        id <- identifier Prelude.False
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
        id <- identifier Prelude.False
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
        return $ DeclNode id (FuncDefNode (Just id) args (SequenceIfNode allCases seqPos) seqPos) pos
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
