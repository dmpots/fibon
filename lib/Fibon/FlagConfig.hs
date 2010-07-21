module Fibon.FlagConfig (
  FlagConfig(..)
)
where

data FlagConfig = FlagConfig {
      configureFlags :: [String]
    , buildFlags     :: [String]
    , runFlags       :: [String]
  } deriving (Show, Eq, Ord)
