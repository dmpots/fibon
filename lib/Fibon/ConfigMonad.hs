{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Fibon.ConfigMonad (
    ConfigParameter(..)
  , ConfigMonad
  , done
  , append
  , replace
)
where

import Control.Monad.State
import qualified Data.Map as Map

data ConfigParameter =
    ConfigureFlags
  | BuildFlags
  | RunFlags
  deriving (Show, Eq, Ord, Enum)

--data Configuration = Configuration {
--      configureFlags :: [String]
--    , buildFlags     :: [String]
--    , runFlags       :: [String]
--  }
newtype GenConfigMonad a = CM (State Configuration a)
  deriving (Monad)
type Configuration = Map.Map ConfigParameter [String]
type ConfigMonad = GenConfigMonad ()

done :: ConfigMonad
done = CM (return ())

replace :: ConfigParameter -> String -> ConfigMonad
replace c f = do
  CM $ modify (Map.insert c [f])

append :: ConfigParameter -> String -> ConfigMonad
append c f = do
  CM $ modify (Map.insertWith (++) c [f])


