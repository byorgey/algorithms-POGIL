#!/usr/bin/env stack
-- stack --resolver lts-19.6 script --package diagrams-lib --package diagrams-pgf --package texrunner

import Diagrams.Prelude
import Diagrams.Backend.PGF.CmdLine

vennModel :: Diagram B
vennModel = mconcat
  [ bigO # centerY # translateX (-15)
  , bigTheta # centerY # translateX 15  -- # translateY (-5)
  , bigOmega # centerY # translateX 45
  , circle 30
  , circle 30 # translateX 30
  , text "$O(n^2)$" # fontSizeL 4 # translate ((-6) ^& 20)
  -- , text "$\\Theta(n^2)$" # fontSizeL 4 # translate (15 ^& 15)
  , text "$\\Omega(n^2)$" # fontSizeL 4 # translate (36 ^& 20)
  ]
  where
    mkColumn = vsep 7 . map (fontSizeL 3 . text . ("$"++) . (++"$"))
    bigO = mkColumn
      [ "6"
      , "n"
      , "2\\sqrt n"
      , "\\frac{n^2 + 2}{n}"
      ]
    bigTheta = mkColumn
      [ "n^2"
      , "\\frac{n^3 + 3}{n}"
      , "2n^2 + n + 1"
      , "\\frac{n^2}{2} - n"
      ]
    bigOmega = mkColumn
      [ "n^3"
      , "n^4 - 3n^2"
      , "\\frac{n^3}{1000}"
      , "2^n"
      ]

main :: IO ()
main = mainWith vennModel
