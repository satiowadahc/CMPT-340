--CMPT 340
--A3
--March 7
--Chad A. Woitas
--caw724
--11137533

--apologies for not seperating this into folders

--Problem 1)
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b] 
unfold p h t x  | p x = []
        | otherwise  = h x : unfold p h t (t x)

myMap :: (a->b) -> [a] -> [b]
myMap f = unfold null (f.head) tail

iterate :: (a->a) -> a -> [a]
iterate f = unfold (const False) f f

-- fcn id and . may be useful

--Problem 2)
altmap :: (a->b) -> (a->b) -> [a] -> [b]
altmap _ _ [] = [] 
altmap f g (x:xs) = f x : (altmap g f xs)

--Problem 3)
luhnDouble :: (Num a)=>[a] -> [a]
luhnDouble (x:xs) = altmap id (*2) (reverse (x:xs))


luhnF [] = []
luhnF (x:xs) = (if x>9 then x-9 else x) : luhnF xs

luhn list = ((sum (luhnF (luhnDouble list))) `mod` 10) == 0


 
--Problem 4
--[(x, y) | x <- [1, 2, 3], y <- [4, 5, 6]] 
--Make that^ with one generator

allpairs :: [a] -> [b] -> [(a,b)]
allpairs [] _ = []
allpairs _ [] = []
allpairs (x:xs) (y:ys) = (x,y) : (allpairs xs (y:ys))

--Problem 5

perfect = map factorSum [1..]

factorSum num = if ((sum (factorCheck num [1 .. (num-1)])) == num)
        then num:factorSum (num+1)
                else factorSum (num+1)

factorCheck _ [] = []
factorCheck num (x:xs) = (if (num `mod` (x) == 0) then x else 0): (factorCheck num xs)

