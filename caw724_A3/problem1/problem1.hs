--CMPT 340
--A3
--March 7
--Chad A. Woitas
--caw724
--11137533



--Problem 1)

unfold p h t x  | p x = [ ]
        | otherwise  = h x : unfold p h t (t x)

map :: (*)=>[a]
map f = unfold (==0) (f) (f)

--iterate :: ( )=>[a]
iterate f = unfold (==[]) (f) (f . f)


--Problem 2)

--altmap f g = unfold