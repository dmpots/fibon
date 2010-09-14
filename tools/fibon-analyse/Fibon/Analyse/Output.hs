module Fibon.Analyse.Output (
    OutputFormat(..)
  , renderTables
  , renderSummaryTable
)
where

import qualified Data.Map as M
import Fibon.Analyse.Metrics
import Fibon.Analyse.Result
import Fibon.Analyse.Tables
import Text.Tabular
import Text.Printf
import qualified Text.Tabular.AsciiArt as Ascii
import qualified Text.Tabular.Csv      as Csv
import qualified Text.Tabular.Latex    as Latex
import qualified Text.Tabular.SimpleText as Simple

data OutputFormat = 
    AsciiArt 
  | SimpleText {separator :: String}
  | Latex
  | Csv

-- | Render a series of tables based on the 'TableSpec'
--   A table is rendered for each column in the 'TableSpec'. The data for the
--   columns are taken from the list of 'ResultColumn' passed to the function.
renderTables :: [ResultColumn a] -> OutputFormat -> TableSpec a  -> String
renderTables [] _fmt _spec = ""
renderTables rs@(baseline:_compares) fmt tableSpec = 
  unlines $ map render tables
  where
    render (c, t) = 
      c ++ "\n" ++
      renderAs fmt (printf fmtString) id pprMetric table
      where table = (Table rowHeader colHeader t)
    tables = map (\c -> (cName c , map (rowData rs c) benchNames)) tableSpec
    rowHeader = Group NoLine rowNames
    colHeader = Group NoLine colNames
    rowNames = map Header benchNames
    colNames = map Header resultNames
    benchNames = M.keys (results baseline)
    resultNames = map resultLabel rs
    fmtString = "%"++(show . maximum $ map length benchNames)++"s"

-- | Render a summary table comparing the "base" to a "peak" result.
--   The table is rendered with a column of "peak" data for each column listed
--   in the 'TableSpec'.
renderSummaryTable::[ResultColumn a] -> OutputFormat -> TableSpec a -> String
renderSummaryTable [base, peak] fmt tableSpec = 
  render summary
  where
    render t =
      "Fibon Summary" ++ "\n" ++
      renderAs fmt (printf fmtString) id pprMetric table
      where table = (Table rowHeader colHeader t)
    summary = 
      map (\bm -> concatMap (\c -> rowData [peak] c bm) tableSpec) benchNames
    rowHeader = Group NoLine rowNames
    colHeader = Group NoLine colNames
    rowNames = map Header benchNames
    colNames = map (Header . cName) tableSpec
    benchNames = M.keys (results base)
    fmtString = "%"++(show . maximum $ map length benchNames)++"s"
renderSummaryTable _ _ _ = ""


rowData :: [ResultColumn a] -> ColSpec a -> BenchName -> [MetricFormat]
rowData resultColumns (ColSpec _ metric) benchName  = metrics
  where
  metrics = map (getMetric . M.lookup benchName . results) resultColumns
  getMetric Nothing  = NoResult
  getMetric (Just a) = toFormat (metric a)


type RenderFun rh ch td =  (rh -> String) -- ^ Row header renderer
                        -> (ch -> String) -- ^ Col header renderer
                        -> (td -> String) -- ^ Data renderer
                        -> Table rh ch td -- ^ Table data
                        -> String         -- ^ Rendered table
renderAs :: OutputFormat -> RenderFun rh ch td
renderAs AsciiArt         = Ascii.render
renderAs Latex            = Latex.render
renderAs Csv              = Csv.render
renderAs (SimpleText sep) = Simple.render sep

