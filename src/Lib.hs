module Lib
    ( someFunc
    , likes
    ) where

someFunc :: Int -> Int -> Int
someFunc a b = a * b

likes :: [String] -> String
likes [] = "no one likes this"
likes [name] = name ++ " likes this"
likes [name0, name1] = name0 ++ " and " ++ name1 ++ " like this"
likes [name0, name1, name2] = name0 ++ ", " ++ name1 ++ " and " ++ name2 ++ " like this"
likes names =
  let
    h = head names
    ht = head $ tail names
    tt = tail $ tail names
  in
    h ++ ", " ++ ht ++ " and " ++ (show $ length tt) ++ " others like this"