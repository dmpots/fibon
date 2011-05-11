module Fibon.InputSize(
  InputSize(..)
)
where

data InputSize =
    Test
  | Train
  | Ref
  deriving(Eq, Read, Show, Ord, Enum)

