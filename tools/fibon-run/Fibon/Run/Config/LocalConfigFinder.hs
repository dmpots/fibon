{-# LANGUAGE  TemplateHaskell #-}
module Fibon.Run.Config.LocalConfigFinder (
   findLocalConfigModules
)
where

import Language.Haskell.TH
import System.FilePath

modulesFileName :: FilePath
modulesFileName = "LocalConfigModules.txt"

readLocalConfigModules :: FilePath -> IO [String]
readLocalConfigModules baseDir = do
  ms <- readFile (baseDir </> modulesFileName)
  return (lines ms)
  where

findLocalConfigModules :: FilePath -> Q Exp
findLocalConfigModules baseDir = do
  ms <- runIO $ readLocalConfigModules baseDir
  return $ ListE $ map (\m -> (VarE . mkName) (m ++ ".config")) ms

