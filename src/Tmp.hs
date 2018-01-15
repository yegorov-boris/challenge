module Tmp
  ( ef
  , evaluateFunction
  , factorial
  , fibonacci
  , coinchange
  , heigth
  , foo
  ) where

import Data.List
-- import Data.Maybe

evaluateFunction :: Ord a => (a -> Either b ([a], [b] -> b)) -> a -> b
evaluateFunction f a =
  let
    findByFst arg = (== arg) . fst
    ef state f a = case f a of
      Left l -> case find findByFst state of
        Nothing -> ((a, l):state, l)
        Just _ -> (state, l)
      Right (args, handler) ->
        let
          processArg s arg = case find (findByFst arg) state of
            Nothing ->
              let
                (currentState, processedArg) = ef s f arg
              in
                ((arg, processedArg):currentState, processedArg)
            Just (k, v) -> (s, v)
          processArgs s result (h:t) =
            let
              (currentState, processedArg) = processArg h
              currentResult = processedArg:result
            in
              if
                null t
              then
                (currentState, currentResult)
              else
                processArgs currentState currentResult t
          (currentState, valsToHandle) = processArgs state [] $ sort args
        in
          (currentState, handler valsToHandle)
  in
    snd $ ef [] f a

factorial i | i == 0    = Left 1
            | otherwise = Right ([i-1], (*i).head)

fibonacci i | i < 2     = Left i
            | otherwise = Right ([i-1, i-2], sum)

coinchange (a, i) | a == 0          = Left 1
                  | a < 0 || i == 0 = Left 0
                  | otherwise       = Right ([(a, i-1), (a-coinlist!!(i-1), i)], sum)
coinlist = [1, 3, 5, 10]

heigth (n, m) | m <= 0 || n <= 0 = Left 0
              | otherwise        = Right ([(n, m-1), (n-1, m-1)], (+1).sum)

foo  i | i <= 2    = Left 1
       | odd i     = Right ([6*i`div`7, 2*i`div`3], sum)
       | otherwise = Right ([i-1, i-3], sum)
