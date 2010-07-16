
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

module SVG
where

import Vectors

svgInit :: Int -> Int -> String
svgInit w h = "<?xml version=\"1.0\"?>\n" ++
              "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n" ++
              "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"" ++ (show w) ++ "\" height=\"" ++ (show h) ++"\">\n"

svgFinish :: String
svgFinish = "</svg>\n"

svgRects :: [Vector] -> String
svgRects []     = ""
svgRects (v:vs) = svgRect v ++ svgRects vs

svgRect :: Vector -> String
svgRect (fx, fy) = "<rect x=\"" ++ (show x) ++ "\" y=\"" ++ (show y) ++  "\" width=\"2\" height=\"2\" style=\"fill:rgb(0,0,255);stroke-width:1;stroke:rgb(0,0,0)\"/>\n"
    where x = fromEnum fx
          y = fromEnum fy

svgLines :: [Vector] -> String
svgLines []     = ""
svgLines (v:vs) | length vs == 0 = []
                | otherwise      = (svgLine v (head vs)) ++ (svgLines vs)

svgLine :: Vector -> Vector -> String
svgLine (fx1, fy1) (fx2, fy2) | fx1 < 2 || fy1 < 2 || fx2 < 2 || fy2 < 2 = ""
                              | otherwise                                =  "<line x1=\"" ++ (show x1) ++ "\" y1=\"" ++ (show y1) ++ "\" x2=\"" ++ (show x2) ++ "\" y2=\"" ++ (show y2) ++ "\" style=\"stroke:rgb(99,99,99);stroke-width:2\"/>\n"
    where x1 = fromEnum fx1
          x2 = fromEnum fx2
          y1 = fromEnum fy1
          y2 = fromEnum fy2

