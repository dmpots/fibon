module Fibon.Analyse.Parse(
    GhcStats(..)
  , parseFibonResults
  , parseBinarySize
)
where

import Data.Char
import qualified Data.Map        as M
import Data.Word
import Fibon.Result
import Fibon.Analyse.ExtraStats
import Fibon.Analyse.Result
import System.FilePath
import Text.Regex


parseFibonResults :: FilePath  -- ^ input file
                  -> IO (Maybe (M.Map ResultLabel [FibonResult])) -- ^ Result
parseFibonResults file = do
  input <- readFile file
  case runParser input of
    Nothing -> return Nothing
    Just [] -> return Nothing
    Just rs -> return (Just $ M.mapKeys addFileSource (groupResults rs))
  where
    baseName        = takeBaseName file
    addFileSource s = baseName ++ s

    runParser :: String -> Maybe [FibonResult]
    runParser text = sequence (map parseLine (lines text))

    parseLine :: String -> Maybe FibonResult
    parseLine line = 
      case reads line of
        [(r, _)] -> Just r
        _        -> Nothing

groupResults :: [FibonResult] -> M.Map ResultLabel [FibonResult]
groupResults = foldr grouper M.empty
  where
  grouper   r = M.insertWith (++)  (benchType r) [r]
  benchType r = dropWhile (/= '-') (benchName r)


-- | Parses the output of the @size@ command.
--   The output is of the form:
--
--    text\t   data\t    bss\t    dec\t    hex
--    817842\t  49536\t  48136\t 915514\t  df83a
--
parseBinarySize :: String -> Maybe Word64
parseBinarySize s =
  case lines s of
    [header, sizes] ->
      let keys   = splitRegex sp (dropWhile isSpace header)
          values = splitRegex sp (dropWhile isSpace sizes)
      in
          lookup "dec" (zip keys values) >>= mbParse
    _ -> Nothing
    where
    mbParse w = case reads w of [(size,_)] -> Just size; _ -> Nothing

sp :: Regex
sp = mkRegex "[ \t]+"

