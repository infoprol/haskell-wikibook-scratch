--
-- https://en.wikibooks.org/wiki/Haskell/Recursion
--



factorial n = go n 1
    where
    go n res
        | n > 1     = go (n - 1) (res * n)
        | otherwise = res



--fact n = fact n 1
fact 1 acc = acc
fact n acc = fact (n - 1) (n * acc)




power _ 0 = 1
power x 1 = x
power x y = (power x (y - 1)) * x




--log2 :: Num -> Int
log2 x = loop x 0
    where
    loop x acc
        | x < 2     = acc
        | otherwise = loop (x / 2) (acc + 1)




rcate :: Int -> a -> [a]
rcate 0 _       = []
rcate n x       = x : (rcate (n - 1) x)





main = do
    putStr "5! = " -- ++ (show $ factorial 5) ++ ".\n"
    print (factorial 5)
    putStr ".\n"