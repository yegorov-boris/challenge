module LispLovesMe
  ( lispEval
  , replace
  ) where

import Data.List (elem, foldl, reverse)

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

replace :: String -> String -> String
replace [] s = s
replace _ [] = []
replace chars s = reverse $ foldl (\result c -> if c `elem` chars then result else c:result) [] s
