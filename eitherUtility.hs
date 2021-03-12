module EitherUtility where

import Text.Megaparsec as P
import Data.Void
import Nodes

foldE f =
    foldr (
        \fa fb -> 
            do
                a <- fa
                b <- fb
                Right $ f a b
        )

mapEW :: (a -> String) -> (b -> b) -> [Either a b] -> Either a [b]
mapEW sf _ [] = Right []
mapEW sf f ls@(_:_) = return $ map (f . nf) ls where
    nf (Right a) = a
    nf (Left e) = error $ sf e

mapE :: (TraversableStream s, VisualStream s, ShowErrorComponent v) => 
    (a -> a) -> [Either (ParseErrorBundle s v) a] -> Either (ParseErrorBundle s v) [a]
mapE = mapEW P.errorBundlePretty

verify :: [Either a ()] -> Either a ()
verify = foldE (\_ b -> b) (Right ())

(|>>) :: Either a b -> Either a bx -> Either a bx
(|>>) = (*>)