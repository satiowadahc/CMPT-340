-- Adds two integers
add :: Integer -> Integer -> Integer
add x y =
    x + y

-- Subtracts two integers
sub :: (Integer,Integer) -> Integer
sub (x,y)=
    x - y

curry :: ((a,b)->c) -> (a->b->c)
curry f a b = f (a,b)

uncurry :: (a->b->c) -> ((a,b)->c)
uncurry f (a,b) = f a b


