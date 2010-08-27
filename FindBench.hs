module FindBench(findLocalBenchmarks) where
--module Main where

import Control.Exception
import Control.Monad (filterM)
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
  ")",
  "where",
  "import Fibon.InputSize",
  "import Fibon.BenchmarkInstance"
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
groupName g = g ++ "Group"

benchDataDecl :: [String] -> String
benchDataDecl []  = ""
benchDataDecl bms =
  "data FibonBenchmark =\n    " ++
  (join ("\n  | ") bms) ++
  "\n    deriving(Show, Eq, Ord, Enum)"

groupDataDecl :: [String] -> String
groupDataDecl []   = ""
groupDataDecl grps = 
  "data FibonGroup =\n    " ++
  (join ("\n  | ") (map groupName grps)) ++
  "\n    deriving(Show, Eq, Ord, Enum)"

allBenchmarksDecl :: [String] -> String
allBenchmarksDecl bms =
  "allBenchmarks :: [FibonBenchmark]\n"++
  "allBenchmarks = [\n      "++
  (join ("\n    , ") bms) ++
  "\n  ]"

benchGroupDecl :: [(String, String)] -> String
benchGroupDecl qBms =
  "benchGroup :: FibonBenchmark -> FibonGroup\n"++
  (join ("\n") $ map defn qBms)
  where
  defn (g,bm) = "benchGroup " ++ bm ++ " = " ++ (groupName g)
  
benchInstanceDecl :: [(String, String)] -> String
benchInstanceDecl qBms =
  "benchInstance :: FibonBenchmark -> InputSize -> BenchmarkInstance\n"++
  (join ("\n") $ map defn qBms)
  where
  defn (g,bm) = "benchInstance " ++ bm ++ " = " ++ (importAs g bm) ++ ".mkInstance"

join :: String -> [String] -> String
join s ss = concat (intersperse s ss)

