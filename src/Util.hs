module Util where

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.:) = (.) . (.)
infixl 7 .:

impossible :: a
impossible = error "Impossible!"
