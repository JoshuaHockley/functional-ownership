module Parser.Util where

import Parser.Monad

import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser Text
symbol = L.symbol space

parenthesised :: Parser a -> Parser a
parenthesised = between (symbol "(") (symbol ")")

bracketed :: Parser a -> Parser a
bracketed = between (symbol "[") (symbol "]")

braced :: Parser a -> Parser a
braced = between (symbol "{") (symbol "}")

semicolonSeparated :: Parser a -> Parser [a]
semicolonSeparated = (`sepBy` symbol ";")

semicolonSeparated1 :: Parser a -> Parser [a]
semicolonSeparated1 = (`sepBy1` symbol ";")

commaSeparated :: Parser a -> Parser [a]
commaSeparated = (`sepBy` symbol ",")
