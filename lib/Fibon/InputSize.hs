module Fibon.InputSize(
  InputSize(..)
)
where

data InputSize =
    Test
  | Ref
  deriving(Eq, Read, Show, Ord, Enum)

