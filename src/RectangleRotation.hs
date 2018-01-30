module RectangleRotation
  ( rectangleRot
  ) where

rectangleRot :: Int -> Int -> Int
rectangleRot a b =
  let
    dMax = floor $ (fromIntegral (a + b)) / (2 * (sqrt 2))
    dA = floor $ (fromIntegral a) / (sqrt 2)
    dB = floor $ (fromIntegral b) / (sqrt 2)
    isInside x y
      | (y <= (x + dB)) && (y <= ((- x) + dA)) && (y >= ((- x) - dA)) && (y >= (x - dB)) = 1
      | otherwise = 0
    checkPoint n x y
      | x > dMax = n
      | y > dMax = checkPoint n (x + 1) (- dMax)
      | otherwise = checkPoint (n + isInside x y) x (y + 1)
  in
    checkPoint 0 (- dMax) (- dMax)
