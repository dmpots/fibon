module Fibon.Analyse.Analysis (
    Analysis(..)
  , runAnalysis
)
where
import qualified Data.Map        as M
import Fibon.Result
import Fibon.Analyse.Parse
import Fibon.Analyse.Result

data Analysis a = Analysis {
      fibonAnalysis :: (FibonResult -> IO FibonStats)-- ^ RunData analyser
    , extraParser   :: (String  -> Maybe a)          -- ^ extraStats parser
    , extraAnalysis :: ([a]     -> IO    a)          -- ^ extraStats analyser
  }

runAnalysis :: Analysis a -> FilePath -> IO (Maybe [ResultColumn a])
runAnalysis analysis file = do
  fibonResults <- parseFibonResults file
  case fibonResults of
    Nothing -> return Nothing
    Just rs -> do x <- createResultColumns analysis rs
                  return (Just x)

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


