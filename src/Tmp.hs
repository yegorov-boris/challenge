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

data Ord a => Tree a b = NilNode | Node (Tree a b) (a, b) (Tree a b) deriving Show

findInTree :: Ord a => Tree a b -> a -> Maybe b
findInTree NilNode _ = Nothing
findInTree (Node tLeft (k, v) tRight) x
  | x == k = Just v
  | x < k = findInTree tLeft x
  | otherwise = findInTree tRight x

insertNode :: (Ord a) => Tree a b -> (a, b) -> Tree a b
insertNode NilNode x = Node NilNode x NilNode
insertNode (Node tLeft (k, v) tRight) (kx, vx)
  | k == kx = Node tLeft (k, v) tRight
  | k < kx = Node tLeft (k, v) (insertNode tRight (kx, vx))
  | otherwise = Node (insertNode tLeft (kx, vx)) (k, v) tRight

type F a b = a -> Either b ([a], [b] -> b)

evaluateFunction :: Ord a => F a b -> a -> b
evaluateFunction f a = case ef NilNode f a of
  Left (state, l) -> l
  Right (state, r) -> r

ef :: Ord a => Tree a b -> F a b -> a -> Either (Tree a b, b) (Tree a b, b)
ef state f a = case f a of
  Left l -> Left (state, l)
  Right (args, handler) ->
    let
      (currentState, valsToHandle) = processArgs f state [] $ sort args
    in
      Right (currentState, handler valsToHandle)

processArgs :: Ord a => F a b -> Tree a b -> [b] -> [a] -> (Tree a b, [b])
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

processArg :: Ord a => F a b -> Tree a b -> a -> (Tree a b, b)
processArg f state arg = case findInTree state arg of
  Nothing ->
    case ef state f arg of
      Left l -> l
      Right (currentState, processedArg) -> (insertNode currentState (arg, processedArg), processedArg)
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
