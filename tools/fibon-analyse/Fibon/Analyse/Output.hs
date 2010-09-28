module Fibon.Analyse.Output (
    OutputFormat(..)
  , renderTables
  , renderSummaryTable
)
where

import qualified Data.Map as M
import Fibon.Analyse.Analysis
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
renderTables :: [ResultColumn a] -> OutputFormat -> [ColSpec a] -> String
renderTables [] _fmt _spec = ""
renderTables rs@(baseline:compares) fmt tableSpec =
  unlines $ map render tableSpec
  where
    render colSpec =
      renderTable (cName colSpec) colNames columns fmt [colSpec]
    columns     = baselineCol : compareCols
    baselineCol = (NormNone, baseline)
    compareCols = [(NormPercent baseline, c) | c <- compares]
    colNames = map resultLabel rs


-- | Render a summary table comparing the "base" to a "peak" result.
--   The table is rendered with a column of "peak" data for each column listed
--   in the 'TableSpec'.
renderSummaryTable::[ResultColumn a] -> OutputFormat -> TableSpec a -> String
renderSummaryTable [base, peak] fmt tableSpec = 
  renderTable "Fibon Summary" colNames columns fmt tableSpec
  where
    columns = [(NormPercent base, peak)]
    colNames = map cName tableSpec
renderSummaryTable _ _ _ = ""

-- | Renders a table. The number of columns is
--   length (tableData) * length (TableSpec).
renderTable :: String                          -- ^ Table name
            -> [String]                        -- ^ Column names
            -> [(Normalize a, ResultColumn a)] -- ^ Table data
            -> OutputFormat                    -- ^ Output format
            -> TableSpec a                     -- ^ Table description
            -> String
renderTable _  _ [] _ _ = ""
renderTable tableName columnNames rs@(aResult:_) fmt tableSpec =
  render table
  where
    render t =
      tableName ++ errMsg ++ "\n" ++
      renderAs fmt (printf fmtString) id (renderPerfData fmt) tab
      where tab = (Table rowHeader colHeader t)
    (errMsg, (rowLabels, table)) =
      case computeRows rs benchNames tableSpec of
        Left err   -> (err, ([],[[]]))
        Right rows -> ("", unzip rows)
    rowHeader = Group NoLine rowNames
    colHeader = Group NoLine colNames
    rowNames = map Header rowLabels
    colNames = map Header columnNames
    benchNames = M.keys (results . snd $ aResult)
    fmtString = "%"++(show . maximum $ map length rowLabels)++"s"


renderPerfData :: OutputFormat -> (PerfData -> String)
renderPerfData = pprPerfData . includeUnits

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


-- | Don't print units for outputs which are intended to be inputs to
--   other analysis programs (like R or Excel)
includeUnits :: OutputFormat -> Bool
includeUnits (SimpleText _) = False
includeUnits Csv            = False
includeUnits _              = True

