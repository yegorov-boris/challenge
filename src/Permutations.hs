module Permutations
 ( permutations
 , swap
 ) where

import Data.List (sort)

permutations :: String -> [String]
permutations s = perm [sort s]

perm :: [String] -> [String]
perm [""] = [""]
perm [[a]] = [[a]]
perm (s:p) =
  let
    n = length s
    leftIndex (-1) = (-1)
    leftIndex i = if s!!i >= s!!(i + 1) then leftIndex (i - 1) else i
    l = leftIndex (n - 2)
    rightIndex i = if s!!l >= s!!i then rightIndex (i - 1) else i
    r = rightIndex (n - 1)
  in
    if
      l == (-1)
    then
      s:p
    else
      let
        xs = swap l r s
        next = (take (l + 1) xs) ++ (reverse $ drop (l + 1) xs)
      in
        perm (next:s:p)

swap :: Int -> Int -> [a] -> [a]
swap l r a =
  let
    (left, (elemL:mr)) = splitAt l a
    (middle, (elemR:right)) = splitAt (r - l - 1) mr
  in
    if l == r then a else left ++ elemR:middle ++ elemL:right
