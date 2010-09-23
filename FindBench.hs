module FindBench(findLocalBenchmarks) where
--module Main where

import Control.Exception
import Control.Monad (filterM, when)
import Data.List
import System.Directory
import System.FilePath
import System.IO

{-
-- for standalone testing
main = do
  findLocalBenchmarks "benchmarks"
-}

benchmarksModule         = ["Fibon", "Benchmarks"]
benchmarksInstanceModule = "Fibon.Instance"

findLocalBenchmarks :: FilePath -> IO ()
findLocalBenchmarks baseDir = do
  let searchPath = join ([pathSeparator]) (baseDir : benchmarksModule)
  putStr $ "Looking for benchmarks in "++searchPath
  groups <- bmGroups searchPath
  bms    <- bmInstances searchPath groups
  let allGroups    = sort            groups
      allBms       = (sort . concat) bms
      qualifiedBms = 
        concat $ zipWith (\g bs -> map ((,)g) (sort bs)) allGroups bms
      outFile      = searchPath ++ ".hs"
  when (null allGroups) printNoBenchmarksWarning
  putStrLn $ "... found ("++ (show.length$ allBms)++")"
  putStrLn $ "  writing benchmark manifest to "++outFile
  h <- openFile outFile WriteMode
  hPutStrLn h moduleHeader
  hPutStrLn h $ moduleImports (join "." benchmarksModule) qualifiedBms
  hPutStrLn h ""
  hPutStrLn h $ benchDataDecl allBms
  hPutStrLn h ""
  hPutStrLn h $ groupDataDecl allGroups
  hPutStrLn h ""
  hPutStrLn h $ allBenchmarksDecl allBms
  hPutStrLn h ""
  hPutStrLn h $ benchGroupDecl qualifiedBms
  hPutStrLn h ""
  hPutStrLn h $ benchInstanceDecl qualifiedBms
  hPutStrLn h ""
  hPutStrLn h $ benchPathDecl qualifiedBms
  hClose h

bmGroups :: FilePath -> IO [FilePath]
bmGroups baseDir = do
  dirs <- try (getDirectoryContents baseDir) :: IO (Either IOError [FilePath])
  case dirs of
    Left  _  -> return [] 
    Right ds -> removeBadEntries baseDir ds

bmInstances :: FilePath -> [FilePath] -> IO [[String]]
bmInstances baseDir groups = do
  let paths = map (baseDir </>) groups
  bms <- mapM getDirectoryContents paths
  mapM (\(p, bm) -> removeBadEntries p bm) (zip paths bms)

removeDotFiles :: [FilePath] -> [FilePath]
removeDotFiles = filter (\d -> not ("." `isPrefixOf` d))

removeBadEntries :: FilePath -> [FilePath] -> IO [FilePath]
removeBadEntries baseDir dirs = do
  let paths = map (baseDir </>) dirs
  noFiles <- filterM (\d -> doesDirectoryExist (baseDir </> d)) dirs
  let noUnderscores = filter (\d -> not ("_" `isPrefixOf` d)) noFiles
  return (removeDotFiles noUnderscores)

moduleHeader :: String
moduleHeader = join "\n" [
  "module "++modName++" (",
  "    FibonBenchmark(..)",
  "  , FibonGroup(..)",
  "  , allBenchmarks",
  "  , benchGroup",
  "  , benchInstance",
  "  , benchPath",
  ")",
  "where",
  "import Fibon.InputSize",
  "import Fibon.BenchmarkInstance",
  "import System.FilePath"
  ]
  where
  modName = join "." benchmarksModule 

moduleImports :: String -> [(String, String)] -> String
moduleImports baseMod bms = join "\n" imports
  where
  imports           = map importStmt bms
  importStmt (g,bm) = 
    "import qualified "
    ++baseMod++"."++g++"."++bm++"."++benchmarksInstanceModule
    ++" as "++(importAs g bm) 

importAs :: String -> String -> String
importAs _grp modu = modu ++ "_bm"

groupName :: String -> String
groupName g = g

benchDataDecl :: [String] -> String
benchDataDecl bms = "data FibonBenchmark = " ++ datas bms ++ derivings
  where derivings = "\n    deriving(Read, Show, Eq, Ord, Enum)"
        datas []  = "NoBenchmarksFound"
        datas bms = "\n   " ++ (join ("\n  | ") bms)

groupDataDecl :: [String] -> String
groupDataDecl grps = "data FibonGroup = " ++ datas grps ++ derivings
  where derivings  = "\n    deriving(Read, Show, Eq, Ord, Enum)"
        datas []   = "NoGroupsFound"
        datas grps = "\n   "++ (join ("\n  | ") (map groupName grps))

allBenchmarksDecl :: [String] -> String
allBenchmarksDecl bms =
  "allBenchmarks :: [FibonBenchmark]\n"++
  "allBenchmarks = [\n      "++
  (join ("\n    , ") bms) ++
  "\n  ]"

benchGroupDecl :: [(String, String)] -> String
benchGroupDecl []   = benchGroupTypeDecl ++ "benchGroup = "++ emptyError
benchGroupDecl qBms = benchGroupTypeDecl ++ (join ("\n") $ map defn qBms)
  where
  defn (g,bm) = "benchGroup " ++ bm ++ " = " ++ (groupName g)

benchGroupTypeDecl :: String
benchGroupTypeDecl = "benchGroup :: FibonBenchmark -> FibonGroup\n"

benchInstanceDecl :: [(String, String)] -> String
benchInstanceDecl []   = benchInstanceTypeDecl ++ "benchInstance = "++ emptyError
benchInstanceDecl qBms = benchInstanceTypeDecl ++ (join ("\n") $ map defn qBms)
  where
  defn (g,bm) =
    "benchInstance " ++ bm ++ " = " ++ (importAs g bm) ++ ".mkInstance"

benchInstanceTypeDecl :: String
benchInstanceTypeDecl =
  "benchInstance :: FibonBenchmark -> InputSize -> BenchmarkInstance\n"

benchPathDecl :: [(String, String)] -> String
benchPathDecl []   = benchPathTypeDecl ++ "benchPath = " ++ emptyError
benchPathDecl qBms = benchPathTypeDecl ++ (join ("\n") $ map defn qBms)
  where
  defn (g,bm) = "benchPath " ++ bm ++ " = " ++ s g ++ " </> " ++ s bm
  s x = "\"" ++ x ++"\""

benchPathTypeDecl :: String
benchPathTypeDecl = "benchPath :: FibonBenchmark -> FilePath\n"

join :: String -> [String] -> String
join s ss = concat (intersperse s ss)

emptyError :: String
emptyError = "error \"No benchmarks found. Need to re-run cabal config step\""

printNoBenchmarksWarning :: IO ()
printNoBenchmarksWarning = do
  putStrLn "\n"
  putStrLn banner
  putStrLn $cap("! No benchmarks found.")
  putStrLn $cap("! You will not be able to run collect results with fibon-run")
  putStrLn banner
  putStrLn ""
  where
  banner = line++"WARNING"++line
  line   = take 30 (repeat '-')
  cap s  = s ++ take ((length banner) - (length s) - 1) (repeat ' ') ++ "!"

