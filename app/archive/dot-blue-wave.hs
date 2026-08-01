{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude

spacing :: Double
spacing = 0.40

gridSize :: Int
gridSize = 35

wave :: Double -> Double
wave x =
  (sin x + 1) / 2

radiusAt :: Double -> Double
radiusAt x = x * 0.01 + 0.01

colorAt :: Double -> Colour Double
colorAt x =
  blend
    (wave x)
    blue
    cyan

cell :: (Double, Double) -> Diagram B
cell (x, y) =
  circle (radiusAt x)
    # fc (colorAt x)
    # lw none
    # translate (r2 (x, y))

positions :: [(Double, Double)]
positions =
  [ (x * spacing, y * spacing)
    | x <- range,
      y <- range
  ]
  where
    range =
      [ -fromIntegral gridSize
        .. fromIntegral gridSize
      ]

artwork :: Diagram B
artwork =
  mconcat $
    map cell positions

picture :: Diagram B
picture =
  artwork
    # centerXY
    # bg black

main :: IO ()
main =
  mainWith picture