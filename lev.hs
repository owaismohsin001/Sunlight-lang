module Lev where

import Data.Array ( listArray, array, (!) )
import Data.List

lev :: (Eq a) => [a] -> [a] -> Int
lev xs ys = levMemo ! (n, m)
  where levMemo = array ((0,0),(n,m)) [((i,j),lev i j) | i <- [0..n], j <- [0..m]]
        n = length xs
        m = length ys
        xa = listArray (1, n) xs
        ya = listArray (1, m) ys
        lev 0 v = v
        lev u 0 = u
        lev u v
          | xa ! u == ya ! v = levMemo ! (u-1, v-1)
          | otherwise        = 1 + minimum [levMemo ! (u, v-1),
                                            levMemo ! (u-1, v),
                                            levMemo ! (u-1, v-1)]

similarity :: String -> [String] -> [(String, Int)]
similarity w ls = zipWith (\a b -> (b, lev a b)) (map (const w) ls) ls

closest :: Int -> String -> [String] -> [String]
closest n w ls = map fst $ sortBy compare $ filter (\(_, v) -> v <= n) $ similarity w ls
