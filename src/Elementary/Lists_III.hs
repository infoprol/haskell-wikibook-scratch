-- Lists_III.hs
-- https://en.wikibooks.org/wiki/Haskell/Lists_III
--


ssum :: [Integer] -> Integer
ssum []         = 0
ssum (x:xs)     = x + ssum xs


-- unnecessary and probably incorrect.
--ffoldr :: (a -> b -> b) -> b -> [a] -> b
--ffoldr f acc []         = acc
--ffoldr f acc (a:b:c:[]) = f a (f b (f c acc))
--ffoldr f acc (x:xs)     = f x (ffoldr f acc xs) --?


-- an identity on lists
idFoldr :: [a] -> [a]
idFoldr = foldr (:) []

-- another identity (maybe??)
--idFoldl :: [a] -> [a]
--idFoldl xs = foldl hh [] xs
--    where
--   hh []   = []
--    hh xs   = foldl (:) [] xs



-- just map...
mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f = foldr (\ x xs -> f x : xs) []


-- playing with laziness...
echoes :: [Int] -> [Int]
echoes = foldr (\ x xs -> (replicate x x) ++ xs) []







--
-- exercises (1st set)
-- 1.1
aand :: [Bool]  -> Bool
aand []         = True
aand (True:bs)  = aand bs
aand _          = False

oor :: [Bool]   -> Bool
oor []          = False
oor (False:bs)  = oor bs
oor _           = True


--
-- 1.2
mmaximum :: Ord a => [a] -> a
mmaximum = foldr1 max


mminimum :: Ord a => [a] -> a
mminimum = foldr1 min


--
-- 1.3
rreverse :: [a] -> [a]
rreverse = foldl (\ xs x -> x:xs) []


-- 
-- 2.1
--sscanr :: (a -> b -> b) ->  b -> [a] -> [b]
--sscanr f    acc     [] = [acc]
--sscanr f    (y:ys)  



--
-- 3.4
ffilter :: (a -> Bool) -> [a] -> [a]
ffilter fpred xs = [ x | x <- xs, fpred x ]

mmap :: (a -> b) -> [a] -> [b]
mmap ptws xs = [ ptws x | x <- xs ]







sscanl :: (b -> a -> b) -> b -> [a] -> [b]
sscanl f x0 xs = loop f xs x0 []
    where
    loop f []       curr acc        = acc ++ [curr]
    loop f (x:xs)   curr acc        = loop f xs (f curr x) (acc ++ [curr])


-- sscanl f acc []     = acc : []
-- sscanl f acc (x:xs) = acc : sscan1 f (f acc x) xs


-- e.g., sscanr (+) 0 [1,2,3] = [6,5,3]
sscanr :: (a -> b -> b) -> b -> [a] -> [b]
--sscanr f x0 xs = recurser f xs x0 []
--    where
sscanr f curr []        = []
sscanr f curr (x:xs)    = (sscanr f (f x curr) xs) ++ [(f x curr)]



--    recurser f (x:xs)    curr []        = (f x curr) : (recurser f xs (f x curr) )
--    recurser f []        curr acc     = curr:acc
--    recurser f (x:xs)     curr acc     =  
