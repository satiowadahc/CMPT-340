--Assignment 1
--Chad Woitas
--CMPT 340
--Jan 31


compose3 :: Double -> Double -> Double -> Double
compose3 = (\f -> (\g -> (\h -> (f g h x))))


