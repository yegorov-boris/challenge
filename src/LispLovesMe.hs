module LispLovesMe
  ( lispEval
  , tokenize
  ) where

import Data.List (elem, foldl, reverse, words)

data AST = I32 Int
         | Sym String
         | Nul
         | Err
         | Lst [AST]
         | Boo Bool
         | Nod AST [AST]
         deriving (Eq, Show)

preludeFunctions :: [(String, [AST] -> AST)]
preludeFunctions =
  [ ("+", foldArgs (+))
  , ("*", foldArgs (*))
  , ("-", foldArgs (-))
  , ("/", foldArgs (/))
  , ("^", pow)
  , (">", cmp (>))
  , ("<", cmp (<))
  , ("!", neg)
  , ("list", Lst)
  , ("size", I32 . length)
  , ("reverse", Lst . reverse)
  , ("..", range)
  , ("==", cmp (==))
  , (">=", cmp (>=))
  , ("<=", cmp (<=))
  , ("!=", cmp (!=))
  , ("if", if')
  ]

-- lispPretty :: String -> Maybe String
-- lispPretty s = undefined

lispEval :: String -> Maybe AST
lispEval s = Just $ I32 42

tokenize :: String -> String -> [String]
tokenize [] s = [s]
tokenize _ [] = []
tokenize separators s =
  let
    separate result c = if c `elem` separators then ' ':result else c:result
  in
    words $ reverse $ foldl separate "" s

pow :: [AST] -> AST
pow [(I32 a), (I32 b)] = I32 (a^b)
pow _ = Err

neg :: [AST] -> AST
neg [(Boo x)] = Boo $ not x
neg _ = Err

if' :: [AST] -> AST
if' [(Boo a), b, c] = if a then b else c
if' _ = Err

range :: [AST] -> AST
range [(I32 a), (I32 b)] = Lst $ map I32 [a..b]
range _ = Err

cmp :: (Int -> Int -> Bool) -> [AST] -> AST
cmp f [(I32 a), (I32 b)] = Boo (f a b)
cmp _ _ = Err

foldArgs :: (Int -> Int -> Int) -> [AST] -> AST
foldArgs f xs =
  if
    length xs > 1
  then
    let
      f' result element = case (result, element) of
        ((I32 a), (I32 b)) -> I32 (f a b)
        _ -> Err
    in
      foldl f' (head xs) (tail xs)
  else
    Err
