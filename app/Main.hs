module Main where

import Lib
import FixedPoint

main :: IO ()
main = do
    putStrLn $ show $ (fix foldr') (+) 0 [42, 73]
