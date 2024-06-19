module Main where

import Syn.Decl
import Syn.Expr
import Ty
import Parser (parseProgram)
import Parser.Error (ParseError)
import Tc (typeCheckProgram)
import Tc.Error (TyError)
import Syn.Pretty ()
import Ty.Pretty ()
import Tc.Error.Pretty ()
import Pretty (prettyLit)

import Optics
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable
import Data.Maybe (fromMaybe, isJust)
import Control.Monad
import Control.Monad.Except
import System.IO (readFile')
import System.Exit
import Options.Applicative hiding (ParseError)
import Prettyprinter
import Prettyprinter.Render.Text (putDoc)

main :: IO ()
main = do
  args <- execParser argsP
  src <- (<> fromMaybe "" args.source) . mconcat <$>
    for args.sourceFiles ((T.pack <$>) . readFile')
  let out = eval src
  putDoc $ pretty out
  when (isError out) $
    exitFailure

data Args = Args
  { sourceFiles :: [FilePath],
    source :: Maybe Text
  }

argsP :: ParserInfo Args
argsP = info (args <**> helper) fullDesc
  where
    args :: Parser Args
    args = Args
      <$> many (strArgument (metavar "FILE" <> help "Source file"))
      <*> optional (strOption (short 'e' <> metavar "CODE" <> help "Source code"))

--------------------------------------------------------------------------------

eval :: Text -> EvalOut
eval src = case runExcept (parseProgram src) of
  Left e -> EvalOut Nothing Nothing (Just e) Nothing
  Right p -> case runExcept (typeCheckProgram p) of
    Left (e, mp') -> EvalOut (Just p) mp' Nothing (Just e)
    Right p' -> EvalOut (Just p) (Just p') Nothing Nothing

data EvalOut = EvalOut
  { parsed :: Maybe (Program ()),
    typeChecked :: Maybe (Program Ty),
    parseError :: Maybe ParseError,
    typeError :: Maybe TyError
  }

isError :: EvalOut -> Bool
isError out
  = isJust out.parseError
  || isJust out.typeError

instance Pretty EvalOut where
  pretty EvalOut {parsed, typeChecked, parseError, typeError}
    = vsep . map (<> line) $
      (case parsed of
        Just p -> [vsep [prettyLit "===== PARSER OUT =====", pretty p]]
        _ -> [])
      ++ (case typeChecked of
        Just p -> [vsep [prettyLit "===== TYPECHECKER OUT =====", prettyTypedSubExprs p]]
        _ -> [])
      ++ (case parseError of
        Just e -> [vsep [prettyLit "===== PARSE ERROR =====", pretty e]]
        _ -> [])
      ++ (case typeError of
        Just e -> [vsep [prettyLit "===== TYPE ERROR =====", pretty e]]
        _ -> [])

prettyTypedSubExprs :: Program Ty -> Doc ann
prettyTypedSubExprs prog = vsep $ map ((<> line') . subExprInfo) progSubExprs
  where
    subExprInfo e = vsep [pretty e, indent 1 (prettyLit ":" <+> (pretty e.ann))]
    progSubExprs = prog ^.. programExprs % cosmosOf subExprs
