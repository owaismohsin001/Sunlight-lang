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
mapE f [] = Right []
mapE f (x:xs) = do
    let remaped = map (\(Right a) -> a) (x:xs)
    return $ map f remaped

verify :: [Either a ()] -> Either a ()
verify = foldE (\_ b -> b) (Right ())

(|>>) :: Either a b -> Either a bx -> Either a bx
(|>>) = (*>)