module Fibon.InputSize(
  InputSize(..)
)
where

data InputSize =
    Test
  | Ref
  deriving(Eq, Show, Ord, Enum)

