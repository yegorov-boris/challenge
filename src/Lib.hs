module Lib
    ( someFunc
    , likes
    , findOdd
    , sumDigPow
    , xo
    , nbYear
    , longestConsec
    , findMissing
    , isPrime
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

-- Given an array, find the int that appears an odd number of times.
-- There will always be only one integer that appears an odd number of times.
findOdd :: [Int] -> Int
findOdd [] = 0 -- make the function safe
findOdd (x:xs) =
  let
    count (acc, filtered) element =
      if
        element == x
      then
        (acc + 1, filtered)
      else
        (acc, element:filtered)
    (n, rest) = foldl count (1, []) xs
  in
    if odd n then x else findOdd rest

-- The number 89 is the first integer with more than one digit that fulfills the property partially introduced
-- in the title of this kata. What's the use of saying "Eureka"? Because this sum gives the same number.
-- In effect: 89 = 8^1 + 9^2
-- The next number in having this property is 135.
-- See this property again: 135 = 1^1 + 3^2 + 5^3
-- We need a function to collect these numbers, that may receive two integers a, b
-- that defines the range [a, b] (inclusive) and outputs a list of the sorted numbers in the range
-- that fulfills the property described above.
sumDigPow :: Int -> Int -> [Int]
sumDigPow a b
  | b <= a = []
  | b < 0 = []
  | a < 0 = sumDigPow 0 b
  | otherwise =
    let
      toDigit c = read [c] :: Int
      sdpIterator (i, acc) c = (i + 1, acc + (toDigit c) ^ i)
      sdp = snd . (foldl sdpIterator (1, 0)) . show
    in
      filter (\n -> n == sdp n) [a..b]

-- Check to see if a string has the same amount of 'x's and 'o's.
-- The method must return a boolean and be case insensitive.
-- The string can contains any char.
xo :: String -> Bool
xo =
  let
    updateDelta delta c
      | c == 'x' || c == 'X' = delta + 1
      | c == 'o' || c == 'O' = delta - 1
      | otherwise = delta
  in
    ((==) 0) . (foldl updateDelta 0)


nbYear :: Int -> Double -> Int -> Int -> Int
nbYear p0 percent aug p
  | p0 < 1 = 0
  | p < 1 = 0
  | otherwise =
    let
      foo year current =
        if
          current >= p
        then
          year
        else
          let
            next = current + aug + (floor $ (fromIntegral current) * 0.01 * percent)
          in
            if next <= current then 0 else foo (year + 1) next
    in
      foo 0 p0

-- You are given an array strarr of strings and an integer k.
-- Your task is to return the first longest string consisting of k consecutive strings taken in the array.
longestConsec :: [String] -> Int -> String
longestConsec strarr k
  | k <= 0 = ""
  | otherwise =
    let
      lc prev a
        | k > length a = prev
        | otherwise =
          let
            cur = foldl (++) "" $ take k a
            next = if length cur > length prev then cur else prev
          in
            lc next $ tail a
    in
      lc "" strarr

-- An Arithmetic Progression is defined as one in which there is a constant difference between the consecutive terms
-- of a given series of numbers. You are provided with consecutive elements of an Arithmetic Progression. There is
-- however one hitch: Exactly one term from the original series is missing from the set of numbers which
-- have been given to you. The rest of the given series is the same as the original AP. Find the missing term.
-- You have to write the function findMissing (list) , list will always be atleast 3 numbers.
-- The missing term will never be the first or last one.
findMissing :: Integral n => [n] -> n
findMissing xs
  | length (take 3 xs) < 3 = 0
  | otherwise = foo (d xs) (tail xs)

foo :: (Ord n, Num n) => n -> [n] -> n
foo prevD xs =
  let
    curD = d xs
    h = head xs
  in
    if
      (abs curD) < (abs prevD)
    then
      h - curD
    else
      if
        (abs curD) > (abs prevD)
      then
        h + prevD
      else
        foo curD (tail xs)

d :: Num n => [n] -> n
d xs = (head $ tail xs) - (head xs)

isPrime :: Integer -> Bool
isPrime x
  | x < 2 = False
  | otherwise =
    let
      border = ceiling $ sqrt $ fromInteger x
      prime i
        | border == x = True
        | i > border = True
        | x `mod` i == 0 = False
        | otherwise = prime $ i + 1
    in
      prime 2