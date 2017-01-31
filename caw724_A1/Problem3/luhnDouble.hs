--Assignment 1
--Chad Woitas
--CMPT 340
--Jan 31


--Use Luhns method to verify Banking information
luhn :: (Integer,Integer,Integer,Integer)-> Bool
luhn (w,x,y,z) = ((luhnDouble w+x + luhnDouble y + z ) `mod` 10 ) ==0


--Doubles and then subtracts 9 iff the result is greater then 9
luhnDouble :: Integer->Integer
luhnDouble d = if (d*2)>9 then (d*2)-9 else (d*2)





