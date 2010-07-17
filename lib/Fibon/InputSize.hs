module Fibon.InputSize(
  InputSize(..)
)
where

data InputSize =
    Ref
  | Test
  deriving(Eq, Show, Ord, Enum)

