module EitherUtility where

foldE f =
    foldr (
        \fa fb -> 
            do
                a <- fa
                b <- fb
                Right $ f a b
        )

mapE :: (b -> b) -> [Either a b] -> Either a [b]
mapE _ [] = Right []
mapE f ls@(_:_) = return $ map (f . (\(Right a) -> a)) ls

verify :: [Either a ()] -> Either a ()
verify = foldE (\_ b -> b) (Right ())

(|>>) :: Either a b -> Either a bx -> Either a bx
(|>>) = (*>)