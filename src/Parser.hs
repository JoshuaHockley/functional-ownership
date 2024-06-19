module Parser where

import Parser.Decl
import Parser.Error
import Syn.Decl

import Data.Text (Text)
import Text.Megaparsec hiding (ParseError)
import Control.Monad.Except

parseProgram :: Text -> Except ParseError (Program ())
parseProgram src = liftEither $ parse programP "" src
