import Parser
import Nodes
import DefCheck
import CodeGen
import JsCodeGen
import EitherUtility
import Modes
import ReduceMethods
import Data.Void
import Data.Char
import MergeDefs
import System.Exit
import System.Environment   
import Data.List
import qualified Data.Set as Set
import Debug.Trace
import System.Process
import Text.Megaparsec as P
import GHC.IO.Encoding

remIncludes =
        do
            P.many Parser.newline
            ls <- Parser.includes Lib
            P.many Parser.newline
            is <- Parser.includes Mod
            return (ls, is)

foldS :: Set.Set String -> [(String, String, String)] -> IO [(Set.Set String, Node)]
foldS _ [] = return []
foldS s [(dir, fn, ftxt)] = sequence [fParse s dir fn ftxt] :: IO [(Set.Set String, Node)]
foldS s ((dir, fn, ftxt):xs) =
    do
        tre@(che, _) <- fParse s dir fn ftxt
        ls <- foldS che xs
        return $ tre : ls

fParse :: Set.Set String -> String -> String -> String -> IO (Set.Set String, Node)
fParse cache dir fn fstr = 
    do
        let str = filter (/= '\t') fstr
        let (libs, incs) = res where
            res = case P.runParser remIncludes fn str of
                Right is -> is
                Left e -> error (P.errorBundlePretty e)
        let (ns, ios) = (
                map extractString (extractList incs), 
                mapM (readFile . (\x -> dir ++ "/" ++ extractString x)) (extractList incs) 
                )
        let lns = filter (not . (`Set.member` cache)) (map extractString (extractList libs))
        let nCache = cache `Set.union` Set.fromList (filter (\ s -> head s == '*') lns)
        txs <- ios
        let 
            sepStatic x = if head s == '*' then "./libs/" ++ tail s ++ "/main.slt" else dir ++ "/" ++ s ++ "/main.slt" where s = extractString x
        libs <- mapM (readFile . sepStatic) (extractList libs)
        let sepStatic s = if head s == '*' then "./libs/" ++ tail s else dir ++ "/" ++ s
        let lbs = zip3 (map sepStatic lns) lns libs
        libs <- foldS nCache lbs
        let ps = zipWith (P.runParser (Parser.parse [])) ns txs
        let ins = mapE id ps :: Either (ParseErrorBundle String Data.Void.Void) [Node]
        case ins of 
            Right xs -> 
                case P.runParser (Parser.parse $ map snd libs ++ xs) fn str of
                    Right n -> return (nCache, n)
                    Left e -> error $ P.errorBundlePretty e
            Left n -> error (P.errorBundlePretty n)

compile :: CompileMode m => m -> String -> String -> IO ()
compile mode fstr fn =
    do
        (_, nd) <- fParse Set.empty "." fn fstr
        let tnd = mergeMultipleNode nd
        case DefCheck.checkDefinitions (Right tnd) Nothing of
            Right n -> writeFile (binGen mode) $ wholeCodeGen mode outName n 
            Left str -> error str

compileFile :: CompileMode m => m -> FilePath -> IO ()
compileFile m fn = readFile fn >>= \f -> compile m f fn

runFileMode :: CompileMode m => m -> String -> IO ()
runFileMode m fn = compileFile m fn *> callCommand (callUtilityBin m)

runMode :: CompileMode m => m -> IO ()
runMode = flip runFileMode "main.slt"

run :: IO ()
run = runMode Lua

runFile :: FilePath -> IO ()
runFile = runFileMode Lua

runFileJIT :: FilePath -> IO ()
runFileJIT = runFileMode LuaJIT

runJIT :: IO ()
runJIT = runMode LuaJIT

runJS :: IO ()
runJS = runFileMode Deno "main.slt"

runFileJS :: FilePath -> IO ()
runFileJS = runFileMode Deno

main :: IO ()
main = 
    do
        args <- getArgs
        dispatch $ map (map toLower) args where 
                dispatch [] = compileFile Lua "main.slt"
                dispatch ["run"] = run
                dispatch ["run", a] = runFile a
                dispatch ["jit"] = runJIT
                dispatch ["jit", a] = runFileJIT a
                dispatch [a] = compileFile Lua a
                dispatch xs = error $ "Unexpected arguments " ++ intercalate ", " xs