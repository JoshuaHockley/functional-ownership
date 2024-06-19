module Syn.Ident where

import Data.Text (Text)
import GHC.Generics (Generic)

newtype Ident = Ident
  { s :: Text
  }
  deriving (Generic, Eq, Ord, Show)
