module Parser.Ty where

import Parser.Monad
import Parser.Ident
import Parser.Span
import Parser.Util
import Syn.Ty
import Ty.Prim

import Text.Megaparsec
import Data.List (singleton)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Control.Monad.Combinators.NonEmpty as NE

qTyP :: Parser QTy
qTyP = spanned $ QTy
  <$> contextP
  <*> tyP

tyP :: Parser Ty
tyP = do
  t <- tyAppP
  optional (symbol "->") >>= \case
    Just _ -> do
      onceness <- choice
        [ FnOncenessAnnotated <$> braced tyP,
          FnOncenessOnce <$ symbol "*",
          pure FnOncenessMany
        ]
      lifetime <- choice
        [ FnLifetimeAnnotated <$>
            (spanned $ Ty . TRegionIntersection <$> regionIntersectionP),
          pure FnLifetimeStatic
        ]
      t' <- tyP
      pure $ Ty (TFn (TyFn t t' lifetime onceness)) (joinSpans t.span t'.span)
    _ -> pure t

tyAppP :: Parser Ty
tyAppP = do
  (t :| ts') <- NE.some aTyP
  pure $ case NE.nonEmpty ts' of
    Nothing -> t
    (Just ts) -> Ty (TApp (TyApp t ts)) (joinSpans t.span (NE.last ts).span)

aTyP :: Parser Ty
aTyP = choice
  [ simple,
    parenthesised tyP
  ]
  where
    simple = spanned $ Ty <$> choice
      [ TPrim <$> primTyP,
        TCon <$> tyConP,
        TVar <$> tyVarP,
        TRef <$> tyRefP,
        TRegionIntersection <$> regionIntersectionP,
        TOnceness <$> oncenessP
      ]

primTyP :: Parser PrimTy
primTyP = choice
  [ PrimBool <$ symbol "Bool",
    PrimInt <$ symbol "Int"
  ]

tyConP :: Parser TyCon
tyConP = TyCon <$> upperIdentP

tyVarP :: Parser TyVar
tyVarP = TyVar <$> lowerIdentP

tyRefP :: Parser TyRef
tyRefP = TyRef
  <$> (symbol "&" *> aTyP)
  <*> tyAppP

regionIntersectionP :: Parser RegionIntersection
regionIntersectionP = RegionIntersection
  <$> bracketed (commaSeparated tyP)

oncenessP :: Parser Onceness
oncenessP = choice
  [ Once <$ symbol "Once",
    Many <$ symbol "Many"
  ]

constraintsP :: Parser Constraints
constraintsP = Constraints <$> choice
  [ parenthesised (commaSeparated (constraintP)),
    singleton <$> constraintP
  ]

constraintP :: Parser Constraint
constraintP = spanned $ Constraint <$> choice
  [ COutlives <$> outlivesConstraintP,
    CClass <$> classConstraintP
  ]

outlivesConstraintP :: Parser OutlivesConstraint
outlivesConstraintP = OutlivesConstraint
  <$> aTyP
  <*> (symbol ">" *> aTyP)

classConstraintP :: Parser ClassConstraint
classConstraintP = ClassConstraint
  <$> classP
  <*> many aTyP

classP :: Parser Class
classP = Class <$> upperIdentP

tySigP :: Parser QTy
tySigP = symbol ":" *> qTyP

contextP :: Parser Constraints
contextP = option (Constraints []) $
  try (constraintsP <* symbol "=>")
