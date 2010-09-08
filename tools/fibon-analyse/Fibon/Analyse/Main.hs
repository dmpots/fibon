module Main (main) where
import qualified Data.ByteString as B
import qualified Data.Map        as M
import Data.Maybe
import Fibon.Analyse.ExtraStats
import Fibon.Analyse.Metrics
import Fibon.Analyse.Parse
import Fibon.Analyse.Tables
import Fibon.Analyse.TableSpec


main :: IO ()
main = putStrLn "hi"

--t1 = basicTable
--
--t3 = [ColumnSpec "time" (Just        . compileTime),
--      ColumnSpec "gc"   (fmap d      . extraStats)
--      ]

-- foo :: [MetricFormat]
-- foo =
--   map (\(Column _ f, r) -> toFormat $ fromJust (f r)) (zip t1 results)
--   where results = concat $ map M.elems $ parseResults (const Nothing) (B.empty) 
-- 
