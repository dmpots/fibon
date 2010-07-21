{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Fibon.ConfigMonad (
    ConfigParameter(..)
  , FlagConfig(..)
  , ConfigMonad
  , done
  , append
  , replace
  , runConfig
  , mergeConfig
)
where

import Control.Monad.State
import qualified Data.Map as Map
import Fibon.FlagConfig

data ConfigParameter =
    ConfigureFlags
  | BuildFlags
  | RunFlags
  deriving (Show, Eq, Ord, Enum)


newtype GenConfigMonad a = CM {configState :: (State ConfigMap a)}
  deriving (Monad)
type ConfigMap   = Map.Map ConfigParameter [String]
type ConfigMonad = GenConfigMonad ()

done :: ConfigMonad
done = CM (return ())

replace :: ConfigParameter -> String -> ConfigMonad
replace c f = do
  CM $ modify (Map.insert c [f])

append :: ConfigParameter -> String -> ConfigMonad
append c f = do
  CM $ modify (Map.insertWith (flip (++)) c [f])

runConfig :: ConfigMonad -> FlagConfig
runConfig c = toFlagConfig finalState
  where
  finalState = execState (configState c) (Map.empty)

mergeConfig :: FlagConfig -> ConfigMonad -> FlagConfig
mergeConfig fc cm = toFlagConfig finalState
  where
  startState = fromFlagConfig fc
  finalState = execState (configState cm) startState

toFlagConfig :: ConfigMap -> FlagConfig
toFlagConfig configMap =
  FlagConfig {
      configureFlags = Map.findWithDefault [] ConfigureFlags configMap
    , buildFlags     = Map.findWithDefault [] BuildFlags configMap
    , runFlags       = Map.findWithDefault [] RunFlags configMap
  }

fromFlagConfig :: FlagConfig -> ConfigMap
fromFlagConfig fc =
    Map.insert ConfigureFlags (configureFlags fc) $
    Map.insert BuildFlags     (buildFlags     fc) $
    Map.insert RunFlags       (runFlags       fc) $
    Map.empty
