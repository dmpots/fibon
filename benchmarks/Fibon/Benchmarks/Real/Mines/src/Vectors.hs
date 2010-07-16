
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

module Vectors
where

import System.Random

type Vector = (Float, Float)

addV :: Vector -> Vector -> Vector
addV (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

vectorDiff :: Vector -> Vector -> Vector
vectorDiff (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

vectorLength :: Vector -> Float
vectorLength (x, y) = sqrt (x*x + y*y)

normalize :: Vector -> Vector
normalize (x, y) | x == 0 && y == 0 = (0, 0)
                 | otherwise        = (x / vectorLength (x, y), y / vectorLength (x, y))

scaleVector :: Float -> Vector -> Vector
scaleVector n (x, y) = (n * x, n * y)

tryShortenVector :: Float -> Vector -> Vector
tryShortenVector n v = if vectorLength v > n then n `scaleVector` normalize v else v

randomVector :: Float -> Float -> IO Vector
randomVector minC maxC = do
    x <- randomCoordinate minC maxC
    y <- randomCoordinate minC maxC
    return (x, y)

randomCoordinate :: Float -> Float -> IO Float
randomCoordinate minC maxC = randomRIO (minC, maxC)

clampVector :: Float -> Float -> Vector -> Vector
clampVector minC maxC (x, y) = (newx, newy)
    where newx = if x < minC then maxC else if x > maxC then minC else x
          newy = if y < minC then maxC else if y > maxC then minC else y

