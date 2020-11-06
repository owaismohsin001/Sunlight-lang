import Parser
import Nodes
import DefCheck
import CodeGen
import EitherUtility
import ReduceMethods
import Data.Void
import MergeDefs
import System.Exit
import System.Environment   
import Data.List
import qualified Data.Set as Set
import Debug.Trace
import Text.Megaparsec as P

strtStr = "local base_path = string.match(arg[0], '^(.-)[^/\\\\]*$')\npackage.path = string.format(\"%s;%s?.lua\", package.path, base_path)\n"
endStr = "if out then out():getOutput() else i = 1; while true do loopout()(SltThunk.create(function() return SltNum.create(i, {1, 1}) end)); i = i+1; end end"

remIncludes =
        do
            P.many Parser.newline
            ls <- Parser.includes Lib
            P.many Parser.newline
            is <- Parser.includes Mod
            return (ls, is)

fParse :: Set.Set String -> String -> String -> String -> IO (Either (ParseErrorBundle String Void) Node)
fParse cache dir fn fstr = 
    do
        let str = filter (\x -> x /= '\t') fstr
        let (libs, incs) = res where
            res = case P.runParser remIncludes fn str of
                Right is -> is
                Left e -> error (P.errorBundlePretty e)
        let (ns, ios) = (
                map extractString (extractList incs), 
                mapM (readFile . (\x -> dir ++ "/" ++ extractString x)) (extractList incs) 
                )
        let lns = filter (not . (`Set.member` cache)) (map extractString (extractList libs))
        let nCache = cache `Set.union` (Set.fromList $ filter (\s -> head s == '*') lns)
        txs <- ios
        let 
            sepStatic x = if head s == '*' then "./libs/" ++ tail s ++ "/main.slt" else dir ++ "/" ++ s ++ "/main.slt" where
                s = extractString x
        libs <- mapM (readFile . sepStatic) (extractList libs)
        let sepStatic s = if head s == '*' then "./libs/" ++ tail s else dir ++ "/" ++ s
        libs <- mapM id (zipWith3 (fParse nCache) (map sepStatic lns) lns libs) :: IO [Either (ParseErrorBundle String Void) Node]
        let ls = res where
            res = case mapM id libs :: Either (ParseErrorBundle String Void) [Node] of
                Right ns -> ns
                Left e -> error (P.errorBundlePretty e)
        let ps = zipWith (P.runParser (Parser.parse [])) ns txs
        let ins = mapE id ps :: Either (ParseErrorBundle String Data.Void.Void) [Node]
        case ins of 
            Right xs -> return $ P.runParser (Parser.parse $ ls ++ xs) fn str
            Left n -> return $ Left n

run :: String -> String -> IO ()
run fstr fn =
    do
        nd <- fParse Set.empty "." fn fstr
        let tnd = res where 
            res = case nd of
                    Left e -> Left $ P.errorBundlePretty e
                    Right n -> Right $ mergeMultipleNode n
        case DefCheck.checkDefinitions tnd Nothing of
            Right n -> writeFile "bin.lua" $ strtStr ++ "require 'SltRuntime'\n" ++ CodeGen.runGenerator (Right n) ++ ";\n\n" ++ endStr
            Left str -> putStrLn str

runFile fn =
    do
        f <- readFile fn
        run f fn

main = 
    do
        args <- getArgs
        let fn = head args
        f <- readFile fn
        run f fn
