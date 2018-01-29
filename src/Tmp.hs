module Tmp
  ( evaluateFunction
  , ef
  , factorial
  , fibonacci
  , coinlist
  , coinchange
  , heigth
  , foo
  ) where

import Data.List

data Tree a b = Tree [Tree a b] a (Maybe b) deriving Show

type State a b = [Tree a b]
type F a b = a -> Either b ([a], [b] -> b)

findInTrees :: Ord a => State a b -> a -> Maybe b
findInTrees [] _ = Nothing
findInTrees ((Tree trees k v):t) x
  | x == k = v
  | x < k = findInTrees trees x
  | otherwise = findInTrees t x

insertTree :: Ord a => a -> (Maybe b) -> State a b -> State a b
insertTree key value [] = [Tree [] key value]
insertTree key value ((Tree trees k v):t)
  | key == k = (Tree trees k value):t
  | key < k = (Tree (insertTree key value trees) k v):t
  | otherwise = (Tree trees k v):(insertTree key value t)

evaluateFunction :: Ord a => F a b -> a -> b
evaluateFunction f a = case ef [Tree [] a Nothing] f a of
  Left (state, l) -> l
  Right (state, r) -> r

ef :: Ord a => State a b -> F a b -> a -> Either (State a b, b) (State a b, b)
ef state f a = case f a of
  Left l -> Left (state, l)
  Right (args, handler) ->
    let
      (currentState, valsToHandle) = processArgs f state [] $ sort args
    in
      Right (currentState, handler valsToHandle)

processArgs :: Ord a => F a b -> State a b -> [b] -> [a] -> (State a b, [b])
processArgs f state result [] = (state, result)
processArgs f state result (h:t) =
  let
    (currentState, processedArg) = processArg f state h
  in
    processArgs f currentState (processedArg:result) t

processArg :: Ord a => F a b -> State a b -> a -> (State a b, b)
processArg f state arg = case findInTrees state arg of
  Nothing ->
    case ef (insertTree arg Nothing state) f arg of
      Left l -> l
      Right (currentState, processedArg) -> (insertTree arg (Just processedArg) currentState, processedArg)
  Just v -> (state, v)

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
