--CMPT 340
--A3
--March 7
--Chad A. Woitas
--caw724
--11137533



--Problem 1)
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b] 
unfold p h t x  | p x = []
        | otherwise  = h x : unfold p h t (t x)


--myMap f x:xs = unfold (==[]) f tail x

iterate f = unfold (==False) f id

-- fcn id and . may be useful

--Problem 2)
altmap _ _ [] = [] 
altmap f g (x:xs) = f x : (altmap g f xs)

--Problem 3)
luhnDouble (x:xs) = altmap id (*2) (reverse (x:xs))

luhnF [] = []
luhnF (x:xs) = (if x>9 then x-9 else x) : luhnF xs

luhn list = ((sum (luhnF (luhnDouble list))) `mod` 10) == 0