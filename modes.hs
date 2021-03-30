module Modes where
import Nodes

class CompileMode m where
    importGen :: m -> String
    codeGen :: m -> Node -> String
    fileNameGen :: m -> String -> String
    startGen :: m -> String
    endGen :: m -> String -> String
    sepGen :: m -> String

    wholeCodeGen :: m -> String -> Node -> String
    wholeCodeGen m out n = 
        case startGen m of 
            "" -> importGen m ++ sepGen m ++ codeGen m n ++ sepGen m ++ endGen m out ++ sepGen m
            xs -> xs ++ sepGen m ++ codeGen m n ++ sepGen m ++ endGen m out ++ sepGen m

    invokeUtility :: m -> String


binGen :: CompileMode m => m -> String
binGen = flip fileNameGen "bin"

callUtility :: CompileMode m => m -> String -> String
callUtility m fn = invokeUtility m ++ " " ++ fn

callUtilityBin :: CompileMode m => m -> String
callUtilityBin m = callUtility m $ binGen m