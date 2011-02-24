module Fibon.Analyse.Analysis (
    Analysis(..)
  , runAnalysis
  , computeRows
  , Normalize(..)
  , NormMethod
)
where
import Data.List
import Data.Maybe
import qualified Data.Map        as M
import Control.Monad.Error
import Fibon.Result
import Fibon.Analyse.AnalysisRoutines
import Fibon.Analyse.Parse
import Fibon.Analyse.Result
import Fibon.Analyse.Metrics
import Fibon.Analyse.Statistics
import Fibon.Analyse.Tables
import qualified Data.Vector.Unboxed as V

runAnalysis :: Analysis a -> FilePath -> IO (Maybe [ResultColumn a])
runAnalysis analysis file = do
  fibonResults <- parse file
  case fibonResults of
    Nothing -> return Nothing
    Just rs -> do x <- createResultColumns analysis rs
                  return (Just x)
  where
    parse f | ".SHOW" `isSuffixOf` f = parseShowFibonResults   f
            | otherwise              = parseBinaryFibonResults f

createResultColumns :: Analysis a
                    -> M.Map ResultLabel [FibonResult] 
                    -> IO [ResultColumn a]
createResultColumns analysis fibonResults =
  mapM (analyseResults analysis) (M.toList fibonResults)
  

analyseResults :: Analysis a
               -> (ResultLabel, [FibonResult])
               -> IO (ResultColumn a)
analyseResults analysis (resultName, fibonResults) = do
  ars <- mapM (analyseResult analysis) fibonResults
  return $ ResultColumn resultName (resMap ars)
  where
    resMap ars       = foldr create M.empty (zip ars fibonResults)
    create (ar,fr) m = M.insert (benchNameOnly fr) ar m
    benchNameOnly fr = takeWhile (/= '-') (benchName fr)
  
analyseResult :: 
                 Analysis a
              -> FibonResult 
              -> IO (AnalyseResult a)        -- ^ final result
analyseResult analysis fibonR = do
  fibonS <- (fibonAnalysis analysis) fibonR
  extraS <- case mbExtras of 
              Nothing -> return   Nothing
              Just [] -> return   Nothing
              Just es -> return . Just =<< (extraAnalysis analysis) es 
  return (AnalyseResult fibonS extraS)
  where
    mbExtras = sequence $ map (extraP.runStats) (details.runData $ fibonR)
    extraP   = extraParser analysis


-- | Functions for normalizing and computing summaries of results
--
--
type RowData = (RowName, [PerfData])
type RowName    = String
type TableError = String
type PerfMonad  = Either TableError
type NormMethod a = ResultColumn a -> Normalize a

data Normalize a =
    NormPercent (ResultColumn a)
  | NormRatio   (ResultColumn a)
  | NormNone

computeRows :: [(Normalize a, ResultColumn a)]
            -> [BenchName]
            -> TableSpec a
            -> Either TableError ([RowData], [RowData])
computeRows resultColumns benchs colSpecs = do
  rows <- mapM (computeOneRow resultColumns colSpecs) benchs
  let colData = transpose $ map snd rows
      doSumm  how = mapM (summarize how) colData
  minRow   <- doSumm Min
  meanRow  <- doSumm GeoMean
  arithRow <- doSumm ArithMean
  maxRow   <- doSumm Max
  let sumRows = [
          ("min", minRow)
        , ("geomean", meanRow)
        , ("arithmean", arithRow)
        , ("max", maxRow)
        ]
  return (rows, sumRows)

computeOneRow :: [(Normalize a, ResultColumn a)]
              -> [ColSpec a]
              -> BenchName
              -> PerfMonad RowData
computeOneRow resultColumns colSpecs bench = do
  row <- mapM (\spec ->
      mapM (computeOneColumn bench spec) resultColumns
    ) colSpecs
  return (bench, concat row)

computeOneColumn :: BenchName
                 -> ColSpec a
                 -> (Normalize a, ResultColumn a)
                 -> PerfMonad PerfData
computeOneColumn bench (ColSpec _ metric) (normType, resultColumn) =
  maybe (return NoResult) doNormalize (getRawPerf resultColumn)
  where
    doNormalize peak =
      case normType of
        NormPercent base -> normToBase base normalizePercent
        NormRatio   base -> normToBase base normalizeRatio
        NormNone         -> return (mkRaw peak)
      where
        mkRaw  = Basic . Raw
        mkNorm = Basic . Norm
        normToBase base normFun = maybe (return NoResult)
                                        (\b -> mkNorm `liftM` normFun b peak)
                                        (getRawPerf base)
    getRawPerf rc = perf $ fmap metric ((M.lookup bench . results) rc)

type NormFun a =(a -> Double) -> a -> a -> NormPerf
normalizePercent :: RawPerf -> RawPerf -> PerfMonad NormPerf
normalizePercent = normalize normP

normalizeRatio :: RawPerf -> RawPerf -> PerfMonad NormPerf
normalizeRatio = normalize normR

normalize :: NormFun RawPerf
          -> RawPerf
          -> RawPerf
          -> PerfMonad NormPerf
normalize n base@(RawTime _) peak@(RawTime _) =
  return(n rawPerfToDouble base peak)
normalize n base@(RawSize _) peak@(RawSize _) =
  return(n rawPerfToDouble base peak)
normalize _ _ _ = throwError "Can not normalize a size by time"

normP :: NormFun a
normP = norm Percent (\base peak -> (peak / base) * 100)

normR :: NormFun a
normR = norm Ratio (\base peak -> (base / peak))

-- TODO: use the intervals to compute the resulting interval
norm :: (Estimate Double -> NormPerf)  -- ^ NormPerf constructor
     -> (Double -> Double -> Double)   -- ^ Normalizing function
     -> (a -> Double)                  -- ^ Conversion to double
     -> a -> a                         -- ^ Values to normalize
     -> NormPerf
norm c f toDouble base peak =
  c (mkPointEstimate mkStddev (f (toDouble base) (toDouble peak)))
  where mkStddev = fromIntegral :: Int -> Double

summarize :: Summary -> [PerfData] -> PerfMonad PerfData
summarize how perfData =
  case (normData, rawData) of
    ([], []) -> return NoResult
    (nd, []) -> summarizeNorm how nd
    ([], rd) -> summarizeRaw  how rd
    _        -> throwError "Mixed raw and norm results in column"
  where
    normData = getData (\p -> case p of Basic (Norm n) -> Just n ; _ -> Nothing)
    rawData  = getData (\p -> case p of Basic (Raw  r) -> Just r ; _ -> Nothing)
    getData  f = (catMaybes . map f) perfData

summarizeRaw :: Summary -> [RawPerf] -> PerfMonad PerfData
summarizeRaw how rawPerfs =
  case (isTime rawPerfs, isSize rawPerfs) of
    (True, _) -> summarizeRaw' how ExecTime          RawTime rawPerfs
    (_, True) -> summarizeRaw' how (MemSize . round) RawSize rawPerfs
    _         -> throwError "Can only summarize column with time or size"
  where
    isTime = all (\r -> case r of RawTime _ -> True; RawSize _ -> False)
    isSize = all (\r -> case r of RawSize _ -> True; RawTime _ -> False)

summarizeRaw' :: Summary                 -- ^ what kind of summary
             -> (Double -> a)            -- ^ rounding function
             -> (Estimate a -> RawPerf)  -- ^ RawPerf constructor
             -> [RawPerf]                -- ^ Performance numbers to summary
             -> PerfMonad PerfData
summarizeRaw' how roundFun makeRaw rawPerfs =
  return $ Summary how (Raw (makeRaw (fmap roundFun (computeSummary how vec))))
  where
    vec = V.fromList (map rawPerfToDouble rawPerfs)

summarizeNorm :: Summary -> [NormPerf] -> PerfMonad PerfData
summarizeNorm how normPerfs =
  case (isPercent normPerfs, isRatio normPerfs) of
    (True, _) -> summarizeNorm' how Percent normPerfs
    (_, True) -> summarizeNorm' how Ratio   normPerfs
    _         -> throwError "Can only summarize column with percent or ratio"
    where
      isPercent = all (\r -> case r of Percent _ -> True;  Ratio _ -> False)
      isRatio   = all (\r -> case r of Percent _ -> False; Ratio _ -> True)

summarizeNorm' :: Summary
              -> (Estimate Double -> NormPerf)
              -> [NormPerf]
              -> PerfMonad PerfData
summarizeNorm' how makeNorm normPerfs =
  return $ Summary how (Norm (makeNorm (computeSummary how vec)))
  where
    vec = V.fromList (map normPerfToDouble normPerfs)
