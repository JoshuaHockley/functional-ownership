module Pretty where

import Prettyprinter
import Data.Text (Text)

prettyLit :: Text -> Doc ann
prettyLit = pretty

nest' :: Doc ann -> Doc ann
nest' = nest 2
