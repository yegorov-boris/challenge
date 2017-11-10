module FixedPoint
 ( fix
 , foldr'
 , reverse'
 ) where

foldr' :: ((t2 -> t1 -> t) -> t -> [t2] -> t1) -> (t2 -> t1 -> t) -> t -> [t2] -> t
foldr' rec f z [] = z
foldr' rec f z (x:xs) = f x (rec f z xs)

reverse' :: ([a] -> [a]) -> [a] -> [a]
reverse' rec [] = []
reverse' rec (x:xs) = (rec xs) ++ [x]

fix :: (a -> a) -> a
fix f = let x = f x in x
