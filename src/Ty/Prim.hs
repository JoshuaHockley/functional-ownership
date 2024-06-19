module Ty.Prim where

import GHC.Generics (Generic)

data PrimTy
  = PrimBool
  | PrimInt
  deriving (Generic, Eq, Show)
