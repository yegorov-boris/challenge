module Tmp
  ( evaluateFunction
  , factorial
  , fibonacci
  , coinchange
  , heigth
  , foo
  ) where

import Data.List
-- import Data.Maybe

evaluateFunction :: Ord a => (a -> Either b ([a], [b] -> b)) -> a -> b
evaluateFunction f a = snd $ ef [] f a

ef :: Ord a => [(a, b)] -> (a -> Either b ([a], [b] -> b)) -> a -> ([(a, b)], b)
ef state f a = case f a of
  Left l -> case findByFst a state of
    Nothing -> ((a, l):state, l)
    Just _ -> (state, l)
  Right (args, handler) ->
    let
      (currentState, valsToHandle) = processArgs f state [] $ sort args
    in
      (currentState, handler valsToHandle)

processArgs :: Ord a => (a -> Either b ([a], [b] -> b)) -> [(a, b)] -> [b] -> [a] -> ([(a, b)], [b])
processArgs f state result (h:t) =
  let
    (currentState, processedArg) = processArg f state h
    currentResult = processedArg:result
  in
    if
      null t
    then
      (currentState, currentResult)
    else
      processArgs f currentState currentResult t

processArg :: Ord a => (a -> Either b ([a], [b] -> b)) -> [(a, b)] -> a -> ([(a, b)], b)
processArg f state arg = case findByFst arg state of
  Nothing ->
    let
      (currentState, processedArg) = ef state f arg
    in
      ((arg, processedArg):currentState, processedArg)
  Just (k, v) -> (state, v)

findByFst :: Ord a => a -> [(a, b)] -> Maybe (a, b)
findByFst arg = find $ (== arg) . fst

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
