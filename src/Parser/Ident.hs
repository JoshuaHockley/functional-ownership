module Parser.Ident where

import Parser.Monad
import Parser.Error
import Parser.Util
import Syn.Ident

import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad

keywords :: Set Text
keywords = Set.fromList
  [ "data",
    "class",
    "instance",
    "let",
    "in",
    "case",
    "of",
    "end",
    "if",
    "then",
    "else",
    "rec",
    "false",
    "true",
    "static"
  ]

lowerIdentP :: Parser Ident
lowerIdentP = lexeme . try $ do
  c <- lowerChar
  cs <- many (alphaNumChar <|> char '_' <|> char '\'')
  let s = T.pack $ c : cs
  when (Set.member s keywords) $
    customFailure $ KeywordCollision s
  pure $ Ident s

upperIdentP :: Parser Ident
upperIdentP = lexeme . try $ do
  c <- upperChar
  cs <- many (alphaNumChar <|> char '\'')
  pure . Ident . T.pack $ c : cs
