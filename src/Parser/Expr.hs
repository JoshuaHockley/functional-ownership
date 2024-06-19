module Parser.Expr where

import Parser.Monad
import Parser.Ident
import Parser.Ty
import Parser.Span
import Parser.Util
import Syn.Expr

import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Foldable1 (foldl1')
import Data.Maybe (isJust)
import qualified Control.Monad.Combinators.NonEmpty as NE

exprP :: Parser (Expr ())
exprP = foldl1' app <$> NE.some aExprP
  where
    app f x = Expr (EApp (App f x)) () (joinSpans f.span x.span)

aExprP :: Parser (Expr ())
aExprP = choice
  [ simple,
    lamP,
    parenthesised exprP
  ]
  where
    simple = spanned $ Expr <$> choice
      [ ELit <$> litP,
        EDataCon <$> dataConP,
        EMove <$> varP,
        ERef <$> refP,
        EDeref <$> derefP,
        EDerefCall <$> derefCallP,
        ELet <$> letP,
        ECase <$> caseP,
        EIf <$> ifP,
        EDeref <$> directDerefP,
        EDerefCall <$> directDerefCallP
      ]
      <*> pure ()

lamP :: Parser (Expr ())
lamP = do
  ((externalCaptures, pats, body), s) <- withSpan $ (,,)
    <$> (symbol "\\" *> option [] (bracketed (many varP)))
    <*> some patP
    <*> (symbol "->" *> exprP)
  let internalCaptures = scanl (\cs pat -> patVars pat <> cs) externalCaptures pats
  let mkLam (captures, pat) e = Expr
        { e = ELam $ Lam { captures, pat, e },
          ann = (),
          span = s
        }
  pure $ foldr mkLam body (zip internalCaptures pats)

litP :: Parser Lit
litP = lexeme . try $ choice
  [ LBool <$> boolP,
    LInt <$> L.decimal
  ]
  where
    boolP = (False <$ symbol "false") <|> (True <$ symbol "true")

dataConP :: Parser DataCon
dataConP = DataCon <$> upperIdentP

varP :: Parser Var
varP = Var <$> lowerIdentP

refP :: Parser Ref
refP = Ref <$> (symbol "&" *> varP)

derefP :: Parser (Deref ())
derefP = Deref <$> (symbol "*" *> aExprP)

derefCallP :: Parser (DerefCall ())
derefCallP = DerefCall <$> (symbol "~" *> aExprP)

letP :: Parser (Let ())
letP = Let
  <$> (symbol "let" *> semicolonSeparated1 letBindingP)
  <*> (symbol "in" *> exprP)

caseP :: Parser (Case ())
caseP = Case
  <$> (symbol "case" *> exprP)
  <*> (symbol "of" *> braced (semicolonSeparated caseBranchP))
  where
    caseBranchP = CaseBranch
      <$> patP
      <*> (symbol "->" *> exprP)

ifP :: Parser (If ())
ifP = If
  <$> (symbol "if" *> exprP)
  <*> (symbol "then" *> exprP)
  <*> (symbol "else" *> exprP)

directDerefP :: Parser (Deref ())
directDerefP = Deref <$> (symbol "." *> implicitRefP)

directDerefCallP :: Parser (DerefCall ())
directDerefCallP = DerefCall <$> (symbol "," *> implicitRefP)

implicitRefP :: Parser (Expr ())
implicitRefP = spanned $ Expr
  <$> (ERef . Ref <$> varP)
  <*> pure ()

--------------------------------------------------------------------------------

letBindingP :: Parser (LetBinding ())
letBindingP = choice
  [ BVar <$> varBindingP,
    BPat <$> patBindingP
  ]

varBindingP :: Parser (VarBinding ())
varBindingP = VarBinding
  <$> (isJust <$> optional (symbol "rec"))
  <*> varP
  <*> optional tySigP
  <*> equationP

patBindingP :: Parser (PatBinding ())
patBindingP = PatBinding
  <$> patP
  <*> equationP

patP :: Parser Pat
patP = choice
  [ simple,
    parenthesised patP
  ]
  where
    simple = spanned $ Pat <$> choice
      [ PLit <$> litP,
        PVar <$> varP,
        PDecon <$> deconPatP
      ]
    deconPatP :: Parser DeconPat
    deconPatP = DeconPat
      <$> (isJust <$> optional (symbol "&"))
      <*> dataConP
      <*> many patP

equationP :: Parser (Expr ())
equationP = symbol "=" *> exprP
