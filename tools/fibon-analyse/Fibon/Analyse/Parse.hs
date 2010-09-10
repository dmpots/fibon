module Fibon.Analyse.Parse(
    GhcStats(..)
  , parseFibonResults
)
where

import Control.Monad
import qualified Data.Map        as M
import qualified Data.Text       as T
import qualified Data.Text.IO    as T
import Fibon.Result
import Fibon.Analyse.Metrics
import Fibon.Analyse.ExtraStats
import Fibon.Analyse.Result
import System.FilePath


parseFibonResults :: FilePath  -- ^ input file
                  -> IO (Maybe (M.Map ResultLabel [FibonResult])) -- ^ Result
parseFibonResults file = do
  input <- T.readFile file
  case runParser input of
    Nothing -> return Nothing
    Just [] -> return Nothing
    Just rs -> return (Just $ M.mapKeys addFileSource (groupResults rs))
  where
    baseName        = takeBaseName file
    addFileSource s = baseName ++ s

    runParser :: T.Text -> Maybe [FibonResult]
    runParser text = sequence (map parseLine (T.lines text))

    parseLine :: T.Text -> Maybe FibonResult
    parseLine line = 
      case reads (T.unpack line) of
        [(r, _)] -> Just r
        _        -> Nothing

groupResults :: [FibonResult] -> M.Map ResultLabel [FibonResult]
groupResults = foldr grouper M.empty
  where
  grouper   r = M.insertWith (++)  (benchType r) [r]
  benchType r = dropWhile (/= '-') (benchName r)


