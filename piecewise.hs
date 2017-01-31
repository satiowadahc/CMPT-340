-- File Name: piecewise.hs
-- Author: David Kreiser
-- Class: CMPT 340 Tutorial 1

-- Implements the piecewise function presented on slide 14
f :: Integer -> Integer
f x | x < 1            = x + 4
    | x >= 1 && x < 4  = 2
    | x >= 4           = x - 5

-- Alternate implementation using 'otherwise' clause
fAlternate :: Integer -> Integer
fAlternate x | x < 1     = x + 4
             | x >= 4    = x - 5
             | otherwise = 2