{-# LANGUAGE  TemplateHaskell, CPP #-}
module Fibon.Run.Config.Local(
  configs
)
where

import Fibon.Run.Config
import Fibon.Run.Config.LocalConfigFinder as LocalConfigFinder

#include "LocalConfigImports.txt"

configs :: [(ConfigId, RunConfig)]
configs = map (\x -> (configId x,x)) ms
  where
  ms = $(findLocalConfigModules "config")

