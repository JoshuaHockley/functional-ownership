module Parser.Span where

import Parser.Monad

import Text.Megaparsec
import GHC.Generics (Generic)

data Span = Span
  { start :: SourcePos,
    end :: SourcePos
  }
  deriving (Generic, Show)

joinSpans :: Span -> Span -> Span
joinSpans s1 s2 = Span
  { start = s1.start,
    end = s2.end
  }

withSpan :: Parser a -> Parser (a, Span)
withSpan p = do
  start <- getSourcePos
  x <- p
  end <- getSourcePos
  pure (x, Span { start, end })

spanned :: Parser (Span -> a) -> Parser a
spanned p = uncurry ($) <$> withSpan p
