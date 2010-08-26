module FindConfig(findLocalConfigs, localConfigsFileName) where
import Data.List
import System.Directory
import System.FilePath
import System.IO

findLocalConfigs :: FilePath -> IO ()
findLocalConfigs cDir = do
  fs <- getDirectoryContents cDir
  putStr "\nLooking for local configuration modules... "
  let modNames = map dropExtension $ filter (".hs" `isSuffixOf`) fs
  let imports  = map importStmt $ modNames
  let modules  = map importAs   $ modNames
  putStrLn $ "found ("++ (show.length$ modNames)++")"
  writeToFile (localConfigsFileName cDir) (imports ++ [modulesList modules])
  where
  importStmt m = "import qualified "++m++" as " ++importAs m
  writeToFile fName contents = do
    h <- openFile fName WriteMode
    putStrLn $ "  writing config manifest to " ++ fName
    --putStrLn $ unlines contents
    hPutStr h (unlines contents)
    hClose h

modulesList :: [String] -> String
modulesList mods = typ ++ lst 
  where
  typ = "localConfigs :: [RunConfig]\n"
  lst = "localConfigs = [" ++ (join ", " cfgs) ++ "]"
  cfgs = map (++".config") mods
  join s = concat . intersperse s

importAs :: String -> String
importAs modName = modName++"_Config"

localConfigsFileName :: FilePath -> FilePath
localConfigsFileName baseDir = baseDir </> "LocalConfigs.txt"

