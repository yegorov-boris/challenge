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

-- preludeFunctions :: [(String, [AST] -> AST)]
-- preludeFunctions =
--   [ ("+", undefined)
--   , ("*", undefined)
--   , ("-", undefined)
--   , ("/", undefined)
--   , ("^", undefined)
--   , (">", undefined)
--   , ("<", undefined)
--   , ("!", undefined)
--   , ("list", undefined)
--   , ("size", undefined)
--   , ("reverse", undefined)
--   , ("..", undefined)
--   , ("==", undefined)
--   , (">=", undefined)
--   , ("<=", undefined)
--   , ("!=", undefined)
--   , ("if", undefined)
--   ]

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
