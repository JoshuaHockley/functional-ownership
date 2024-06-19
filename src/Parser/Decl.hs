module Parser.Decl where

import Parser.Monad
import Parser.Expr
import Parser.Ty
import Parser.Span
import Parser.Util
import Syn.Decl
import Syn.Expr

import Text.Megaparsec

programP :: Parser (Program ())
programP = Program
  <$> sepBy declP (notFollowedBy bodyDelim *> symbol ";")
  <*  bodyDelim
  <*> exprP
  <*  eof
  where
    bodyDelim = symbol ";;"

declP :: Parser (Decl ())
declP = spanned $ Decl <$> choice
  [ DBinding <$> bindingDeclP,
    DData <$> dataDeclP,
    DClass <$> classDeclP,
    DInstance <$> instanceDeclP
  ]

bindingDeclP :: Parser (VarBinding ())
bindingDeclP = varBindingP

dataDeclP :: Parser DataDecl
dataDeclP = DataDecl
  <$> (symbol "data" *> tyConP)
  <*> many tyVarP
  <*> braced (semicolonSeparated dataConSigP)
  where
    dataConSigP = DataConSig
      <$> dataConP
      <*> tySigP

classDeclP :: Parser ClassDecl
classDeclP = ClassDecl
  <$> (symbol "class" *> classP)
  <*> many tyVarP
  <*> braced (semicolonSeparated memberSigP)
  where
    memberSigP = MemberSig
      <$> varP
      <*> tySigP

instanceDeclP :: Parser (InstanceDecl ())
instanceDeclP = InstanceDecl
  <$> (symbol "instance" *> contextP)
  <*> classConstraintP
  <*> braced (semicolonSeparated memberImplP)
  where
    memberImplP = MemberImpl
      <$> varP
      <*> equationP
