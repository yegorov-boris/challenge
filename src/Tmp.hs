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

type F a b = a -> Either b ([a], [b] -> b)
type State a b = ([(a, b)], Either Int Int)

evaluateFunction :: Ord a => F a b -> a -> b
evaluateFunction f a = case ef ([], Right 0) f a of
  Left (state, l) -> l
  Right (state, r) -> r

ef :: Ord a => State a b -> F a b -> a -> Either (State a b, b) (State a b, b)
ef state f a = case f a of
  Left l ->
    let
      newDepth = case snd state of
        Right depth -> Left depth
        Left depth -> Left depth
    in
      Left ((fst state, newDepth), l)
  Right (args, handler) ->
    let
      newDepth = case snd state of
        Right depth -> Right (depth + 1)
        Left depth -> Left depth
      (currentState, valsToHandle) = processArgs f (fst state, newDepth) [] $ sort args
    in
      Right (currentState, handler valsToHandle)

processArgs :: Ord a => F a b -> State a b -> [b] -> [a] -> (State a b, [b])
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

processArg :: Ord a => F a b -> State a b -> a -> (State a b, b)
processArg f state arg = case findState arg state of
  Nothing ->
    case ef state f arg of
      Left l -> l
      Right (currentState, processedArg) ->
        case findState arg currentState of
          Nothing ->
            let
              newPairs = drop ((length currentState) - 50) $ (arg, processedArg):(fst currentState)
            in
              ((newPairs, snd currentState), processedArg)
          Just _ -> (currentState, processedArg)
  Just (k, v) -> (state, v)

findState :: Ord a => a -> State a b -> Maybe (a, b)
findState arg = (find ((== arg) . fst)) . fst

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
