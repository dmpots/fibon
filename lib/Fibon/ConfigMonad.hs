module Fibon.ConfigMonad (
    ConfigParameter
  , ConfigMonad
)
where

import Control.Monad.State

data ConfigParameter =
    ConfigurationFlags
  | BuildFlags
  | RunFlags

data Configuration = Configuration {
      configureFlags :: [String]
    , buildFlags     :: [String]
    , runFlags       :: [String]
  }

newtype ConfigMonad a = ConfigMonad (State Configuration a)



