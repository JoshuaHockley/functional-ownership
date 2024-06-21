module Pretty where

import Prettyprinter
import Prettyprinter.Render.String
import Data.Text (Text)
import Debug.Trace (traceShowWith)

prettyLit :: Text -> Doc ann
prettyLit = pretty

nest' :: Doc ann -> Doc ann
nest' = nest 2

showPretty :: Pretty a => a -> String
showPretty = renderString . layoutCompact . pretty

tracePretty :: Pretty a => a -> a
tracePretty = traceShowWith showPretty
