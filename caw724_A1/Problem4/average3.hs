--Assignment 1
--Chad Woitas
--CMPT 340
--Jan 31


average3 :: Integer -> Integer -> Integer -> Integer 
average3 x y z = (x + y + z) `div` 3


howManyAboveAverage :: Integer -> Integer -> Integer -> Integer
howManyAboveAverage x y z = (if x > ave then 1 else 0) + 
                            (if y > ave then 1 else 0) + 
                            (if z > ave then 1 else 0)
                                where ave = average3 x y z

averageThreeInOne (a,b,c) = average3 a b c

orderTriple :: (Integer,Integer,Integer) -> (Integer,Integer,Integer)
orderTriple (m,n,o)  =  if (m<=ave && n==ave && o>ave) then (m,n,o)
                                else if (m<=ave && n>ave && o==ave) then (m,o,n)
                                        else if (m>ave && n==ave && o<=ave) then (o,n,m)
                                                else if (m==ave && n>ave && o<=ave) then (o,m,n)
                                                        else if (m>=ave && n<ave && o<=ave) then (n,o,m)  
                                                                else if (m==ave && n<=ave && o>ave) then (n,m,o) 
                                                                        else if m==n && n==o then (m,n,o)
                                                                                else (0,0,0)
                                                                        where ave = average3 m n o


