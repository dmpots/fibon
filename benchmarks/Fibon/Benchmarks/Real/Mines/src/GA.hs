
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

module GA
where

import System.Random

type GeneType = Float
type Gene = GeneType
type Chromosome = [Gene]
type Population = [Chromosome]

newGene :: GeneType -> GeneType -> IO GeneType
newGene min_ max_ = randomRIO (min_, max_)

newChromosome :: GeneType -> GeneType -> Int -> IO Chromosome
newChromosome min_ max_ n = sequence $ take n $ repeat $ newGene min_ max_

newPopulation :: GeneType -> GeneType -> Int -> Int -> IO Population
newPopulation min_ max_ n m = sequence $ take m $ repeat $ newChromosome min_ max_ n

crossOver :: Chromosome -> Chromosome -> IO Chromosome
crossOver m f = do
    n <- randomRIO (0, (length m - 1))
    return (take n m ++ drop n f)

mixGenes :: [Gene] -> [Gene] -> IO [Gene]
mixGenes []     _      = return []
mixGenes _      []     = return []
mixGenes (m:ms) (f:fs) = do
    n <- randomRIO (0 :: Float, 1)
    let this = if n < 0.5 then m else f
    rest <- mixGenes ms fs
    return (this : rest)

mutate :: Float -> GeneType -> GeneType -> GeneType -> GeneType -> Chromosome -> IO Chromosome
mutate prob minmut maxmut minwg maxwg c = sequence $ map (mutateGene prob minmut maxmut minwg maxwg) c

mutateGene :: Float -> GeneType -> GeneType -> GeneType -> GeneType -> Gene -> IO Gene
mutateGene prob minmut maxmut minwg maxwg g = do
    mut <- randomRIO (0, 1 :: Float)
    if mut > prob 
        then return g
        else do
          r <- randomRIO (minmut, maxmut)
          let insval = if (r + g) < minwg then minwg else if (r + g) > maxwg then maxwg else (r + g)
          return (insval)

pickRoulette :: (Ord a, Num a, Random a) => [a] -> IO Int
pickRoulette pts = do
    let total = sum pts
    if total == 0 
        then do
            (randomRIO (0, (length pts) - 1))
        else do
            p <- randomRIO (0, total)
            let i = pickRoulette' pts p 0
            return (i)

pickRoulette' :: (Ord a, Num a) => [a] -> a -> Int -> Int
pickRoulette' []     _ i = i
pickRoulette' (n:ns) v i = if n < v then pickRoulette' ns (v - n) (i + 1) else i
