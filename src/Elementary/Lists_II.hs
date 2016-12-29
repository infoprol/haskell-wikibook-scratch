-- Lists_II.hs
-- https://en.wikibooks.org/wiki/Haskell/Lists_II
--

import Data.List


takeInt :: Int -> [a] -> [a]
takeInt n xs = reverse $ loop n xs []
    where
    loop _  []      acc = acc
    loop 0  xs      acc = acc
    loop n  (x:xs)  acc = loop (n - 1) xs (x:acc)


dropInt :: Int -> [a] -> [a]
dropInt _ []        = []
dropInt 0 xs        = xs
dropInt n (x:xs)    = dropInt (n - 1) xs    


sumInt :: [Int] -> Int
sumInt xs = loop xs 0
    where
    loop []     acc     = acc
    loop (x:xs) acc     = loop xs (x + acc)


scanSum :: [Int] -> [Int]
scanSum xs = loop xs []
    where
    loop []     acc     = reverse acc
    loop (x:xs) []      = loop xs [x]
    loop (x:xs) (y:ys)  = loop xs ((x + y):y:ys)     


diffs :: [Int] -> [Int]
diffs []        = []
diffs [x]       = []
diffs (x:y:ys)  = (y - x) : diffs (y:ys)


divxss :: [Int] -> [[Int]]
divxss = map (\p -> [ f | f <- [1..p], p `mod` f == 0 ])


-- this type doesn't work here.
--rleEncode :: [Char] -> [(Int * Char)]
rleEncode cs = map f (group cs)
    where
    f (c:css) = (length css + 1, c)


-- factoring out akward "where" clause...
-- evidently the Eq t => type class makes the difference...
rleEncode :: Eq t => [t] -> [(Int, t)]
rleEncode' cs = map (\(c:css) -> (length css + 1, c)) (group cs)


-- e.g., rleDecode [(4,'a'),(3,'b'),(6,'a'),(1,'b'),(1,'a'),(3,'b')]
-- lambda is not the most efficient possible, i'd guess? :P
rleDecode :: Eq t => [(Int, t)] -> [t]
rleDecode = concat . map (\(n, c) -> [ c | i <- [1..n] ])
















