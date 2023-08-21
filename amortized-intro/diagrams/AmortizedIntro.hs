#!/usr/bin/env stack
-- stack --resolver lts-19.6 script --package diagrams-lib --package diagrams-pgf --package texrunner --package colour --package containers

{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module AmortizedIntro where

import           Data.Colour.SRGB
import           Diagrams.Backend.PGF.CmdLine
import           Diagrams.Prelude

toBinary :: Int -> [Int]
toBinary 0 = []
toBinary n = n `mod` 2 : toBinary (n `div` 2)

tex a = text ("$" ++ a ++ "$")

drawCounter :: Int -> Int -> Diagram B
drawCounter len n = hsep 0.5
  [ tex (show n) # fontSizeL 0.5 <> square 1 # lw none
  , drawBitArray len (toBinary n)
  ]

drawBitArray :: Int -> [Int] -> Diagram B
drawBitArray len bs = (hcat . map drawCell)
  (replicate (len - length bs) 0 ++ reverse (take len bs))
  where
    drawCell b = text ("$" ++ show b ++ "$") # fontSizeL 0.9 <> square 1

bitArrayDia = vsep 0.5
  [ hcat (map drawIndex [5, 4 .. 0]) # translateX 1.5
  , vsep 0.5 (map (drawCounter 6) [0..15])
  ]
  where
    drawIndex i = mconcat
      [ text ("\\texttt{" ++ show i ++ "}")
        # fontSizeL 0.5
      , square 1 # lw none
      ]


main = do
  renderPGF "bit-array.pgf" (mkWidth 100) bitArrayDia
