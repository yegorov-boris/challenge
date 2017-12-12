module HammingNumbers
 ( hamming
 ) where

hamming  :: Int -> Int
hamming n
  | n <= 0 = 0
  | n == 1 = 1
  | otherwise =
    let
      ham i pool =
        if
          i == n
        then
          last pool
        else
          let
            newV = minimum [x * y | x <- pool, y <- [2, 3, 5], x * y > (last pool)]
          in
            ham (i + 1) $ dropWhile (\v -> v * 5 <= newV) (pool ++ [newV])
    in
      ham 1 [1]
