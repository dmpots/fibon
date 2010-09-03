{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Fibon.ConfigMonad (
    ConfigParameter(..)
  , FlagConfig(..)
  , Configuration(..)
  , ConfigMonad
  , done
  , append
  , replace
  , setTimeout
  , runWithInitialFlags
)
where

import Control.Monad.State
import qualified Data.Map as Map
import Fibon.FlagConfig
import Fibon.Timeout

data ConfigParameter =
    ConfigureFlags
  | BuildFlags
  | RunFlags
  deriving (Show, Eq, Ord, Enum)


newtype GenConfigMonad a = CM {configState :: (State ConfigState a)}
  deriving (Monad)
data ConfigState = ConfigState {flagS :: ConfigMap, timeoutS :: Timeout}
data Configuration = Configuration {flags :: FlagConfig, timeout :: Timeout}
type ConfigMap   = Map.Map ConfigParameter [String]
type ConfigMonad = GenConfigMonad ()

done :: ConfigMonad
done = CM (return ())

replace :: ConfigParameter -> String -> ConfigMonad
replace p f = do
  CM $ modify $ (\c -> c {flagS = Map.insert p [f] (flagS c)})

append :: ConfigParameter -> String -> ConfigMonad
append p f = do
  CM $ modify $ (\c -> c {flagS = Map.insertWith (flip (++)) p as (flagS c)})
  where as = words f

setTimeout :: Timeout -> ConfigMonad
setTimeout t = do
  CM $ modify $ (\c -> c {timeoutS = t})

runWithInitialFlags :: FlagConfig -> ConfigMonad -> Configuration
runWithInitialFlags fc cm = toConfig finalState
  where
  startState = ConfigState {flagS = fromFlagConfig fc, timeoutS = Infinity}
  finalState = execState (configState cm) startState

toConfig :: ConfigState -> Configuration
toConfig state = Configuration {
    flags =
      FlagConfig {
          configureFlags = Map.findWithDefault [] ConfigureFlags configMap
        , buildFlags     = Map.findWithDefault [] BuildFlags configMap
        , runFlags       = Map.findWithDefault [] RunFlags configMap
      }
    ,
    timeout   = (timeoutS state)
  }
  where
    configMap = flagS state

fromFlagConfig :: FlagConfig -> ConfigMap
fromFlagConfig fc =
    Map.insert ConfigureFlags (configureFlags fc) $
    Map.insert BuildFlags     (buildFlags     fc) $
    Map.insert RunFlags       (runFlags       fc) $
    Map.empty
