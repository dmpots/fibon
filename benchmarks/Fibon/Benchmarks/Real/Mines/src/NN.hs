
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

module NN
where

import System.Random

type Weight = Float
type Neuron = [Weight]
type NeuronLayer = [Neuron]
type NInput = [Weight]
type NOutput = Weight

data NeuralNet = NeuralNet { layers       :: [NeuronLayer]
                           , numinputs    :: Int
                           , numoutputs   :: Int
                           } 
                 deriving (Eq, Show, Read)

getWeights :: NeuralNet -> [Weight]
getWeights n = concat $ concat $ (layers n)

newNeuron :: Weight -> Weight -> Int -> IO Neuron
newNeuron min_ max_ n = sequence $ take n (repeat $ randomRIO (min_, max_))

newNeuronLayer :: Weight -> Weight -> (Int, Int) -> IO NeuronLayer
newNeuronLayer min_ max_ (n, m) = sequence $ take n $ repeat $ newNeuron min_ max_ m

newNeuralNet :: [Int] -> Weight -> Weight -> IO NeuralNet
newNeuralNet ls min_ max_ = do
    newls <- sequence $ map (newNeuronLayer min_ max_) (makePairs ls)
    let nin = snd $ head (makePairs ls)
    let nou = fst $ last (makePairs ls)
    return (NeuralNet newls nin nou)

makePairs :: [Int] -> [(Int, Int)]
makePairs []     = []
makePairs (n:ns) | length ns == 0 = []
                 | otherwise      = (head ns, n) : (makePairs ns)

putWeights :: NeuralNet -> [Weight] -> NeuralNet
putWeights n ws = n{layers=putWeights' (layers n) ws}

putWeights' :: [[Neuron]] -> [Weight] -> [NeuronLayer]
putWeights' []     _  = []
putWeights' (l:ls) ws = putWeightsToLayer l (take (length (concat l)) ws) : putWeights' ls (drop (length (concat l)) ws)

putWeightsToLayer :: NeuronLayer -> [Weight] -> NeuronLayer
putWeightsToLayer []      _  = []
putWeightsToLayer (n:ns)  ws = putWeightsToNeuron n (take (length n) ws) : putWeightsToLayer ns (drop (length n) ws)

putWeightsToLayer' :: NeuronLayer -> [[Weight]] -> NeuronLayer
putWeightsToLayer' _ ws = ws

putWeightsToNeuron :: Neuron -> [Weight] -> Neuron
putWeightsToNeuron _ w = w

updateNeuralNet :: (Weight -> Weight) -> NInput -> NeuralNet -> [NOutput]
updateNeuralNet f i n = foldl (updateNeuronLayer f) i (layers n)

updateNeuronLayer :: (Weight -> Weight) -> NInput -> NeuronLayer -> [NOutput]
updateNeuronLayer f i ns = map (updateNeuron f i) ns

updateNeuron :: (Weight -> Weight) -> NInput -> Neuron -> NOutput
updateNeuron f i n = f $ sum $ map (uncurry (*)) (zip n i)

sigmoid :: Float -> Float -> Float
sigmoid p a = recip (1 + exp(-a/p))
