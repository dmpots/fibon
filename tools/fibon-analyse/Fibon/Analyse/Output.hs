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
renderTables :: [ResultColumn a] -> NormMethod a -> OutputFormat -> [ColSpec a] -> String
renderTables [] _ _fmt _spec = ""
renderTables rs@(baseline:compares) normMethod fmt tableSpec =
  unlines $ map render tableSpec
  where
    render colSpec =
      renderTable (cName colSpec) colNames columns fmt [colSpec]
    columns     = baselineCol : compareCols
    baselineCol = (NormNone baseline, baseline)
    compareCols = [(normMethod baseline, c) | c <- compares]
    colNames = map resultLabel rs


-- | Render a summary table comparing the "base" to a "peak" result.
--   The table is rendered with a column of "peak" data for each column listed
--   in the 'TableSpec'.
renderSummaryTable::[ResultColumn a] -> NormMethod a -> OutputFormat -> TableSpec a -> String
renderSummaryTable [base, peak] normMethod fmt tableSpec =
  renderTable "Fibon Summary" colNames columns fmt tableSpec
  where
    columns = [(normMethod base, peak)]
    colNames = map cName tableSpec
renderSummaryTable _ _ _ _ = ""

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
    (errMsg, dataRowLabels, summaryRowLabels, table) =
      case computeRows rs benchNames tableSpec of
        Left err   -> (err, [],[],[[]])
        Right rows -> unzipRows rows
    unzipRows (dataRows, summRows) =
      let (dl,dr) = unzip dataRows
          (sl,sr) = unzip summRows
      in
      (noErrorMsg, dl,sl, dr ++ sr)
    rowLabels = dataRowLabels ++ summaryRowLabels
    rowHeader = Group SingleLine [
                  (Group NoLine dataRowNames),(Group NoLine summaryRowNames)
                ]
    colHeader = Group NoLine colNames
    dataRowNames = map Header dataRowLabels
    summaryRowNames = map Header summaryRowLabels
    colNames = map Header columnNames
    benchNames = M.keys (results . snd $ aResult)
    fmtString = "%"++(show . maximum $ map length rowLabels)++"s"
    noErrorMsg = ""


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
includeUnits Latex          = False
includeUnits _              = True

