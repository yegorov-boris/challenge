module Ones
  ( countOnes
  ) where

countOnes :: Integer -> Integer -> Integer
countOnes 0 0 = 0
countOnes 0 1 = 1
countOnes 0 b = countOnes 1 b
countOnes 1 b
  | b == ((ceilBinPow b) - 1) = sumChoose $ ceilBinLog b
  | otherwise = (sumChoose $ floorBinLog b) + (countOnes (floorBinPow b) b)
countOnes a b
  | a == b = weight 0 a
  | otherwise =
    let
      border = floorBinPow b
    in
      if
        a < border
      then
        (countOnes a (border - 1)) + (countOnes border b)
      else
        (b - a + 1) + (countOnes (a - border) (b - border))

weight :: Integral t => t -> t -> t
weight w 0 = w
weight w a = weight (w + (a `mod` 2)) $ a `div` 2

ceilBinPow :: Integral t => t -> t
ceilBinPow = (2 ^) . floorBinLog

floorBinPow :: Integral t => t -> t
floorBinPow = (2 ^) . floorBinLog

ceilBinLog :: Integral t => t -> t
ceilBinLog = ceiling . binLog

floorBinLog :: Integral t => t -> t
floorBinLog = floor . binLog

binLog :: (Integral a, Floating b) => a -> b
binLog = logBase 2 . fromIntegral

sumChoose :: Integral t => t -> t
sumChoose n = sum $ map (\k -> k * (choose n k)) [1..n]

choose :: Integral t => t -> t -> t
choose _ 0 = 1
choose 0 _ = 0
choose n k = choose (n-1) (k-1) * n `div` k
