module Parser.Error where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec hiding (ParseError)
import Prettyprinter (Pretty, pretty)

type ParseError = ParseErrorBundle Text SyntaxError

instance Pretty ParseError where
  pretty = pretty . T.pack . errorBundlePretty

data SyntaxError
  = KeywordCollision Text
  deriving (Show, Eq, Ord)

instance ShowErrorComponent SyntaxError where
  showErrorComponent = show
