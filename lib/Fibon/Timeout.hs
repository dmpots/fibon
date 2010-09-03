module Fibon.Timeout(
    Timeout(..)
  , timeoutToMicroSeconds
)
where

data Timeout =
    Infinity
  | Limit {hours :: Int, mins :: Int, secs :: Int}

timeoutToMicroSeconds :: Timeout -> Int
timeoutToMicroSeconds Infinity      = error "Can not make the infinite finte"
timeoutToMicroSeconds (Limit h m s) =
  (hoursToMicroSeconds h) + (minsToMicroSeconds m) + (secsToMicroSeconds s)
  where
    hoursToMicroSeconds = (* (36 * 10 ^ (8::Int)))
    minsToMicroSeconds  = (* (6  * 10 ^ (7::Int)))
    secsToMicroSeconds  = (* (1  * 10 ^ (6::Int)))
  
