module Main where

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude hiding (centerX, centerY, fillColor, height, size, square, width)

cIndigo, cCoral, cMustard, cTeal, cFabric :: Colour Double
cIndigo = sRGB 0.10 0.15 0.35
cCoral = sRGB 0.90 0.40 0.35
cMustard = sRGB 0.85 0.65 0.20
cTeal = sRGB 0.20 0.55 0.50
cFabric = sRGB 0.96 0.95 0.92

artwork :: Diagram B
artwork =
  let tileSize = 2.0
      numCols = 6
      numRows = 6
      width = tileSize * fromIntegral numCols + tileSize
      height = tileSize * fromIntegral numRows + tileSize
   in (tiles tileSize numCols numRows) <> (background width height)

tiles :: Double -> Int -> Int -> Diagram B
tiles tileSize numCols numRows =
  let tileAt i j =
        let x = fromIntegral j * tileSize
            y = fromIntegral i * tileSize

            depth = 3 + ((i * 3 + j * 5) `mod` 3)

            c1 = if even (i + j) then cIndigo else cCoral
            c2 = if (i * j) `mod` 3 == 0 then cMustard else cTeal
         in tile tileSize depth c1 c2
              # translate (r2 (x, y))

      grid = mconcat [tileAt i j | i <- [0 .. numRows - 1], j <- [0 .. numCols - 1]]

      centerX = (fromIntegral (numCols - 1) * tileSize) / 2
      centerY = (fromIntegral (numRows - 1) * tileSize) / 2
      centeredGrid = grid # translate (r2 (-centerX, -centerY))
   in centeredGrid

tile :: Double -> Int -> Colour Double -> Colour Double -> Diagram B
tile size depth color1 color2 =
  let square i =
        let s = size * ((1 / sqrt 2) ^ i)
            rot = if odd i then 45 @@ deg else 0 @@ deg
            color = if even i then color1 else color2

            fillColor =
              if i == depth - 1
                then withOpacity color 1.0 -- または withOpacity color1 1.0
                else transparent
         in rect s s
              # rotate rot
              # lw 2
              # lc color
              # fcA fillColor
   in mconcat [square i | i <- [0 .. depth - 1]]

background :: Double -> Double -> Diagram B
background width height =
  rect width height
    # fc cFabric
    # lw none

main :: IO ()
main = mainWith artwork
