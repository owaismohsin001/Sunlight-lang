import Parser
import DefCheck
import CodeGen
import EitherUtility
import ReduceMethods
import Data.Void
import MergeDefs
import System.Exit
import System.Environment   
import Data.List
import Text.Megaparsec as P

strtStr = "local base_path = string.match(arg[0], '^(.-)[^/\\]*$')\npackage.path = string.format(\"%s;%s?.lua\", package.path, base_path)\n"

run :: String -> String -> IO ()
run str fn =
    do
        let incs = P.runParser Parser.includes fn str
        let (ns, ios) = res where 
            res =
                case incs of 
                    Right is -> ( map extractString (extractList is), mapM readFile (map extractString (extractList is)) )
                    Left e -> error (P.errorBundlePretty e)
        txs <- ios
        let ps = zipWith (P.runParser (Parser.parse [])) ns txs
        let ins = mapE id ps :: Either (ParseErrorBundle String Data.Void.Void) [Node]
        let nd = res where 
            res = 
                case ins of 
                    Right xs -> P.runParser (Parser.parse xs) fn str
                    Left n -> Left n
        let tnd = res where 
            res = case nd of
                    Left e -> Left $ P.errorBundlePretty e
                    Right n -> Right $ mergeMultipleNode n
        case DefCheck.checkDefinitions tnd Nothing of
            Right n -> writeFile "bin.lua" $ strtStr ++ "require 'SltRuntime'\n" ++ CodeGen.runGenerator (Right n) ++ "\n\noutErr(out)"
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