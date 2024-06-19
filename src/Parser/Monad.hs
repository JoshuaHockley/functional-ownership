module Parser.Monad where

import Parser.Error

import Data.Text (Text)
import Text.Megaparsec

type Parser = Parsec SyntaxError Text
