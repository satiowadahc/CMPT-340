--CMPT 340
--Chad A. Woitas
--Assignment 2
--Professor Jamali
--Feb 14

{-
Question 1)
and true (not true)
\v.\w.(v w v) true (not true)
\w.(true w true)(not true)
true (not true) 
\.x\.y (y) (not true)
\.y (y) (not true)
not true
\.v\.w\.x (v x w) true
\.w\.x (true x w)
\.w\.x ((\.a\.b(a)) x w)
\.w\.x ((\.b(x)) w)
\.w\.x (x)
\.x\.y (y)
false
-}

--Question 2)
curry :: ((a,b)->c) -> (a->b->c)
curry f a b = f (a,b)

uncurry :: (a->b->c) -> ((a,b)->c)
uncurry f (a,b) = f a b

--Question 3)

type Exponent = Integer
type Mantissa = Integer

data MyFloat = MyFloat(Mantissa,Exponent)

instance Show MyFloat where
        show (MyFloat(a,b)) = "0." ++ show(a) ++ "x10^" ++ show(b)

--todo recursive call it - add to a list for b count
-- whole :: MyFloat->Integer
-- whole (MyFloat(a,b)) = 

--todo recursive call it -add to a list from right side for 
-- fraction :: MyFloat->Integer
-- fraction (MyFloat(a,b)) = 

instance Num MyFloat where
        (+) = myFloatAdd
        (-) = myFloatSub
        (*) = myFloatMult
        negate = myFloatNeg

--todo figure out different exponent methods
myFloatAdd :: MyFloat -> MyFloat -> MyFloat
myFloatAdd (MyFloat(m1,e1)) (MyFloat(m2,e2)) = 
        if (e1==e2) then (MyFloat(m1+m2,e1)) else
                if(e1<e2) then MyFloat(m1,e1) else
                        if (e1>e2) then (MyFloat(m1,e1)) else (MyFloat(m2,e2))

--todo figure out different exponent methods
myFloatSub :: MyFloat -> MyFloat -> MyFloat
myFloatSub (MyFloat(m1,e1)) (MyFloat(m2,e2)) =
        if (e1==e2) then (MyFloat(m1-m2,e1)) else
                if (e1<e2) then (MyFloat(m1,e1)) else
                        if (e1>e2) then MyFloat(m1,e1) else (MyFloat(m2,e2))

myFloatMult :: MyFloat -> MyFloat -> MyFloat
myFloatMult (MyFloat(m1,e1)) (MyFloat(m2,e2)) = 
        MyFloat(m1*m2,e1+e2)

myFloatNeg :: MyFloat -> MyFloat
myFloatNeg (MyFloat(m1,e1)) = (MyFloat(m1*(-1),e1))

instance Fractional MyFloat where
        (/) = myFloatDiv

myFloatDiv :: MyFloat -> MyFloat -> MyFloat
myFloatDiv (MyFloat(m1,e1)) (MyFloat(m2,e2)) = 
        (MyFloat(m1 `div` m2,e1-e2))

instance Eq MyFloat where
        (==) = myFloatEq

myFloatEq :: MyFloat -> MyFloat -> Bool
myFloatEq (MyFloat(exp1,man1)) (MyFloat(exp2,man2)) = exp1==exp2 && man1==man2

instance Ord MyFloat where
        (<=) = myFloatLEq
        (>=) = myFloatGEq
        (>)  = myFloatGreat
        (<)  = myFloatLess

myFloatLEq :: MyFloat -> MyFloat -> Bool
myFloatLEq (MyFloat(exp1,man1)) (MyFloat(exp2,man2)) = (exp1<=exp2 && man1<=man2)

myFloatGEq :: MyFloat -> MyFloat -> Bool
myFloatGEq (MyFloat(exp1,man1)) (MyFloat(exp2,man2)) = (exp1>=exp2 && man1>=man2)

myFloatGreat :: MyFloat -> MyFloat -> Bool
myFloatGreat(MyFloat(exp1,man1)) (MyFloat(exp2,man2)) = 
        if exp1>exp2 then True else
                if exp1==exp2 && man1>man2 then True else False

myFloatLess :: MyFloat -> MyFloat -> Bool
myFloatLess (MyFloat(exp1,man1)) (MyFloat(exp2,man2)) = 
        if exp1<exp2 then True else
                if exp1==exp2 && man1<man2 then True else False



--Question 4)

shuffle :: [a]->[a]->[a]
shuffle l1 l2 = (head l1):(head l2):l3
        where l3 = (shuffle (drop 1 l1) (drop 1 l2))

--Question 5)

split :: [a]->Integer->[a]->[a]
split l1 n = l2, l3





