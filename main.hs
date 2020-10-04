import Parser
import DefCheck
import CodeGen
import EitherUtility
import ReduceMethods
import Data.Void
import MergeDefs
import System.Exit
import Text.Megaparsec as P

run :: String -> IO ()
run str =
    do
        let incs = P.runParser Parser.includes "<repl>" str
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
                    Right xs -> P.runParser (Parser.parse xs) "<repl>" str
                    Left n -> Left n
        let tnd = res where 
            res = case nd of
                    Left e -> Left $ P.errorBundlePretty e
                    Right n -> Right $ mergeMultipleNode n
        case DefCheck.checkDefinitions tnd Nothing of
            Right n -> putStrLn $ CodeGen.runGenerator $ Right n
            Left str -> putStrLn str
