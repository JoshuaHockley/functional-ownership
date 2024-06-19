module Builtins where

import Ty
import Ty.FromSyn (qTyFromSyn)
import Syn.Expr
import Syn.Ident
import Parser.Ty

import Data.Text (Text)
import qualified Data.Text as T
import Data.Either (fromRight)
import qualified Text.Megaparsec as P
import Optics

builtinVars :: [QTyped Var]
builtinVars = map (typedValue %~ Var . Ident) . parseTys $
  [ "clone" :- "&r a ->[r] a",

    "not" :- "Bool -> Bool",
    "and" :- "Bool -> Bool -> Bool",
    "or" :- "Bool -> Bool -> Bool",

    "negate" :- "Int -> Int",
    "add" :- "Int -> Int -> Int",
    "sub" :- "Int -> Int -> Int",
    "mul" :- "Int -> Int -> Int",
    "div" :- "Int -> Int -> Int",
    "lt" :- "Int -> Int -> Bool",
    "lte" :- "Int -> Int -> Bool",
    "eq" :- "Int -> Int -> Bool",
    "gte" :- "Int -> Int -> Bool",
    "gt" :- "Int -> Int -> Bool",

    "newVec" :- "Unit -> Vec a",
    "len" :- "&r Vec a ->[r] Int",
    "index" :- "Int -> &r Vec a ->[r] &r a",
    "push" :- "a -> Vec a ->* Vec a",
    "update" :- "Int -> (a ->* a) -> Vec a ->* Vec a ",
    "map" :- "(a -> a) -> Vec a -> Vec a",

    "castLifetime" :- "r > s => &r a -> &s a",

    "undefined" :- "Unit -> a"
  ]

builtinDataCons :: [QTyped DataCon]
builtinDataCons = map (typedValue %~ DataCon . Ident) . parseTys $
  [ "Unit" :- "Unit",
    "Pair" :- "a -> b ->* Pair a b"
  ]

--------------------------------------------------------------------------------

parseTys :: [GTyped Text a] -> [QTyped a]
parseTys = map (typedTy %~ parseSafeTy)

parseSafeTy :: Text -> QTy
parseSafeTy s = qTyFromSyn $ fromRight
  (error ("Bad builtin type: " <> T.unpack s))
  (P.parse qTyP "" s)
