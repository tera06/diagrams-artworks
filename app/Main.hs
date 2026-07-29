{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG

main :: IO ()
main =
  renderSVG "circle.svg" (mkWidth 400) diagram

diagram :: Diagram B
diagram =
  circle 1
    # fc blue
    # lw thick