--Assignment 1
--Chad Woitas
--CMPT 340
--Jan 31

--Fast exponetial solver using cases
fastExp1 :: (Integer,Integer)-> Integer
fastExp1 (n,k)= if k==1 then n else
                if k==0 then 1 else
                if k<0 then 0 else
                if even k then (fastExp1 (n,k `div` 2))^2 else n * fastExp1 (n,k-1)

--Fast exponetial solver using guards
fastExp2 :: (Integer,Integer)-> Integer
fastExp2 (n,k)  | k==1   = n
                | k==0   = 1
                | k<0    = 0
                | even k = (fastExp2(n,k `div` 2))^2 
                | otherwise = n * fastExp2 (n,k-1)

--Fast exponetial solver using pattern matching
fastExp3 :: (Integer,Integer)-> Integer
fastExp3 (n,1)  = n
fastExp3 (n,0)  = 1
fastExp3 (n,k) | k<0 = 0
               | even k = (fastExp3(n,k `div` 2))^2 
               | otherwise = n * fastExp3 (n,k-1)
