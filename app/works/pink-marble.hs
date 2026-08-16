{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude hiding (clamp, deform, lerp, pink)

canvasWidth :: Double
canvasWidth = 800

canvasHeight :: Double
canvasHeight = 800

numRowPoints :: Int
numRowPoints = 400

numColPoints :: Int
numColPoints = 400

clamp :: Double -> Double
clamp x
  | x < 0 = 0
  | x > 1 = 1
  | otherwise = x

smoothstep :: Double -> Double
smoothstep x =
  let t = clamp x
   in t * t * (3 - 2 * t)

lerp :: Double -> Double -> Double -> Double
lerp a b t = a + (b - a) * t

lerpColor :: Colour Double -> Colour Double -> Double -> Colour Double
lerpColor c1 c2 t =
  let RGB r1 g1 b1 = toSRGB c1
      RGB r2 g2 b2 = toSRGB c2
      s = smoothstep t
   in sRGB (lerp r1 r2 s) (lerp g1 g2 s) (lerp b1 b2 s)

marblePalette :: Double -> Colour Double
marblePalette t =
  let x = clamp t
      cWhite = sRGB 0.98 0.95 0.96
      cPink = sRGB 0.93 0.62 0.72
      cMagenta = sRGB 0.82 0.38 0.58
      cPurple = sRGB 0.48 0.28 0.52
   in if x < 0.35
        then lerpColor cWhite cPink (x / 0.35)
        else
          if x < 0.70
            then lerpColor cPink cMagenta ((x - 0.35) / 0.35)
            else lerpColor cMagenta cPurple ((x - 0.70) / 0.30)

noise1 :: Double -> Double -> Double
noise1 x y =
  0.50 * sin (1.8 * x + 2.3 * y)
    + 0.25 * sin (3.5 * x - 1.9 * y + 1.2)
    + 0.15 * sin (6.1 * x + 4.7 * y + 2.8)
    + 0.10 * sin (11.2 * x - 8.3 * y)

noise2 :: Double -> Double -> Double
noise2 x y =
  0.48 * cos (2.1 * x - 1.5 * y + 0.8)
    + 0.27 * sin (4.2 * x + 3.1 * y - 1.4)
    + 0.15 * cos (7.8 * x - 5.2 * y + 2.1)
    + 0.10 * sin (12.4 * x + 9.1 * y)

warp1 :: Double -> Double -> (Double, Double)
warp1 x y =
  let qx = noise1 (x + 0.0) (y + 0.0)
      qy = noise2 (x + 1.3) (y + 2.8)
   in (x + 0.35 * qx, y + 0.35 * qy)

warp2 :: Double -> Double -> (Double, Double)
warp2 x y =
  let (qx, qy) = warp1 x y
      rx = noise1 (qx + 3.4) (qy + 1.7)
      ry = noise2 (qx + 5.2) (qy + 4.1)
   in (x + 0.45 * rx, y + 0.45 * ry)

marbleField :: Double -> Double -> Double
marbleField x y =
  let (wx, wy) = warp2 (x * 2.5) (y * 2.5)
      val = noise1 (wx * 2.0) (wy * 2.0)
   in clamp ((val + 1.0) / 2.0)

cell :: Int -> Int -> Diagram B
cell i j =
  let u = fromIntegral i / fromIntegral (numColPoints - 1)
      v = fromIntegral j / fromIntegral (numRowPoints - 1)

      val = marbleField u v

      px = (u - 0.5) * canvasWidth
      py = (0.5 - v) * canvasHeight

      w = canvasWidth / fromIntegral numColPoints
      h = canvasHeight / fromIntegral numRowPoints

      c = marblePalette val
   in rect w h
        # fc c
        # lc c
        # translate (r2 (px, py))

background :: Diagram B
background =
  rect canvasWidth canvasHeight
    # fc (sRGB 0.98 0.95 0.96)
    # lw none

marbleCells :: Diagram B
marbleCells =
  mconcat
    [ cell i j
    | j <- [0 .. numRowPoints - 1],
      i <- [0 .. numColPoints - 1]
    ]

artwork :: Diagram B
artwork =
  (marbleCells <> background)

main :: IO ()
main =
  mainWith
    ( artwork
        # centerXY
    )