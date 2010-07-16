
{-
Copyright (c) 2008 Antti Salonen

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
-}

module Main
where

import Data.List
import Data.Maybe
import System.IO
import System.Directory
import Control.Monad.State
import System.Random

import GA
import NN
import SVG
import Vectors

minCoord :: Float
minCoord = 0

maxCoord :: Float
maxCoord = 400

numMines :: Int
numMines = 50

numSweepers :: Int
numSweepers = 8

numCycles :: Int
numCycles = 1500

numInputs :: Int
numInputs = 4

numOutputs :: Int
numOutputs = 2

minWeight :: Float
minWeight = -1

maxWeight :: Float
maxWeight = 1

svgRate :: Int
svgRate = 10

stdNet :: [Int]
stdNet = [numInputs, 3, 3, numOutputs]

appName :: String
appName = "mines"

data Minesweeper = Minesweeper { location   :: Vector
                               , direction  :: Vector
                               , brain      :: NeuralNet
                               , score      :: Int
                               }
                   deriving (Show, Eq, Read)

type Mine = Vector

data World = World { minesweepers :: [Minesweeper]
                   , minefield    :: [Mine]
                   }
           deriving (Show, Eq, Read)

sortByScore :: [Minesweeper] -> [Minesweeper]
sortByScore []     = []
sortByScore (m:ms) = sortByScore (filter (\x -> score x > score m) ms) ++ [m] ++ sortByScore (filter (\x -> score x <= score m) ms)

createWorld :: Int -> Int -> IO World
createWorld s m = do
    ms <- sequence $ take s $ repeat $ createMinesweeper
    mf <- sequence $ take m $ repeat $ createMine
    return $ World ms mf

createMinesweeper :: IO Minesweeper
createMinesweeper = do
    rl <- randomVector minCoord maxCoord
    rd <- randomVector minCoord maxCoord
    rc <- newNeuralNet stdNet minWeight maxWeight
    return $ Minesweeper rl rd rc 0

createBrainedMinesweeper :: NeuralNet -> IO Minesweeper
createBrainedMinesweeper b = do
    rl <- randomVector minCoord maxCoord
    rd <- randomVector minCoord maxCoord
    return $ Minesweeper rl rd b 0

distanceToNearestMine :: [Mine] -> Vector -> Float
distanceToNearestMine ms l | null ms   = 0
                           | otherwise = minimum (map vectorLength (map (vectorDiff l) ms))

vectorToNearestMine :: [Mine] -> Vector -> Vector
vectorToNearestMine ms l = vectorDiff (nearestMine ms l) l

nearestMine :: [Mine] -> Vector -> Mine
nearestMine ms l = let vectordifflist = (map vectorLength (map (vectorDiff l) ms))
                       mindiff        = if null vectordifflist then 0 else minimum vectordifflist
                       minindex       = elemIndex mindiff vectordifflist
                   in if isNothing minindex then (0,0) else ms !! (fromJust minindex)

createMine :: IO Mine
createMine = randomVector minCoord maxCoord

main :: IO ()
main = do
    let g = mkStdGen 42
    setStdGen g
    w <- createWorld numSweepers numMines
    appdir <- getAppUserDataDirectory appName
    dir <- doesDirectoryExist appdir
    when (not dir) (createDirectory appdir)
    worldLoop 0 w
    return ()

worldLoop :: Int -> World -> IO World
worldLoop g w = do
    appdir <- getAppUserDataDirectory appName
    putStrLn ("Generation " ++ (show g))
    let (neww, path) = updateWorldCycles [(location (head (minesweepers w)))] numCycles w
    let drawsvg = g `mod` svgRate == 0
    when drawsvg $ do
        h <- openFile (appdir ++ "/gen" ++ (show g) ++ ".svg") WriteMode
        hPutStrLn h (svgInit (fromEnum (maxCoord - minCoord)) (fromEnum (maxCoord - minCoord)))
        hPutStrLn h (svgRects (minefield w))
        hPutStrLn h (svgLines (nub (path)))
        hPutStrLn h svgFinish
        hClose h
    putStrLn ("Mines swept: " ++ (show (getScores neww)))
    let sortedms = sortByScore (minesweepers neww)
    putStrLn ("Scores of the best three sweepers: " ++ show (take 3 (map score sortedms)))
    putStrLn ("Mines left: " ++ (show (length (minefield neww))))
    newbrains <- newGeneration (map score sortedms) (map brain sortedms)
    putStrLn ("Weights of the next neural network: " ++ show (head newbrains))
    newsweepers <- mapM createBrainedMinesweeper newbrains
    newmines <- sequence $ take numMines $ repeat $ createMine
    worldLoop (g + 1) (neww{minesweepers = newsweepers, minefield=newmines})

updateWorldCycles :: [Vector] -> Int -> World -> (World, [Vector])
updateWorldCycles v n w | n < 1     = (w, v)
                        | otherwise = updateWorldCycles (snd (updateWorld w) : v) (n -1) (fst (updateWorld w))

updateWorld :: World -> (World, Vector)
updateWorld w = (World newss newf, (location (head newss)))
    where (newss, newf) = updateMinesweepers (minesweepers w) (minefield w)

updateMinesweepers :: [Minesweeper] -> [Mine] -> ([Minesweeper], [Mine])
updateMinesweepers []     f = ([], f)
updateMinesweepers (s:ss) f = (donems, snd (updateMinesweepers ss newf))
    where donems       = news : fst (updateMinesweepers ss newf)
          (news, newf) = updateMinesweeper f s

updateMinesweeper :: [Mine] -> Minesweeper -> (Minesweeper, [Mine])
updateMinesweeper ms s = (news, newmines)
    where news      = Minesweeper newloc newdir (brain s) newscore
          newloc    = clampVector minCoord maxCoord ((location s) `addV` (newdir))
          newdir    = normalize $ (newx, newy)
          newx      = cos newangle
          newy      = sin newangle
          newangle  = oldangle + anglediff
          oldangle  = atan2 (snd (direction s)) (fst (direction s))
          anglediff = (head output) * 0.6
          newscore  = if mineswept then (score s) + 1 else (score s)
          output    = updateNeuralNet (tanh) input (brain s)
          input     = [fst (direction s), snd (direction s), fst (vectorToNearestMine ms (location s)), snd (vectorToNearestMine ms (location s))]
          mineswept = if (not (null ms)) && distanceToNearestMine ms (newloc) < 1.0 then True else False
          newmines  = if mineswept then (delete (nearestMine ms newloc) ms) else ms

newGeneration :: [Int] -> [NeuralNet] -> IO [NeuralNet]
newGeneration ss ns = do
    let ws  = map getWeights ns
    let parents = ws
    cs <- makeChildren ss parents (numSweepers)
    dn <- newNeuralNet stdNet minWeight maxWeight
    let nw = map (putWeights dn) cs
    return (nw)

makeChildren :: [Int] -> [Chromosome] -> Int -> IO [Chromosome]
makeChildren _  _  0 = return []
makeChildren ss ns v = do
    f <- pickRoulette ss
    m <- pickRoulette ss
    c <- makeChild (ns !! m) (ns !! f)
    rest <- makeChildren ss ns (v - 1)
    return (c : rest)

makeChild :: Chromosome -> Chromosome -> IO Chromosome
makeChild m f = do
    c1 <- mixGenes m f
    c2 <- mutate 0.05 (-0.5) 0.5 minWeight maxWeight c1
    return c2

getScores :: World -> [Int]
getScores w = map score (minesweepers w)

clamp :: Ord a => a -> a -> a -> a
clamp mn mx n = if n < mn then mn else if n > mx then mx else n
