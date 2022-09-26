#!/usr/bin/env stack
-- stack --resolver lts-19.6 script --package diagrams-lib --package diagrams-pgf --package texrunner

-- Run with -w 300

import           Diagrams.Backend.PGF.CmdLine
import           Diagrams.Prelude

import           Graphs

g1 :: Graph
g1 = Graph
  (zip (map (:[]) "sabtc") [ (0,4), (0,1), (2,0), (4,1), (3,4) ])
  [ ("s", "a", Just 3), ("s", "c", Just 9), ("a", "b", Just 2)
  , ("b", "t", Just 4), ("c", "t", Just 2)
  ]

g2 :: Graph
g2 = Graph
  (zip (map (:[]) "s234567t") [(0,0), (3,3), (15,3), (13,0), (9,-1), (6,0), (3, -3), (15,-3)])
  [ ("s", "2", Just 9), ("s", "7", Just 15), ("s", "6", Just 14)
  , ("6", "7", Just 5), ("2", "3", Just 24), ("6", "3", Just 18)
  , ("6", "5", Just 30), ("5", "3", Just 2), ("5", "4", Just 11)
  , ("4", "3", Just 6), ("4", "t", Just 6), ("3", "t", Just 19)
  , ("7", "t", Just 44), ("7", "5", Just 20), ("5", "t", Just 16)
  ]

dia :: Diagram B
dia = vsep 5
  [ hsep 2 [drawG (unweight g1), drawG g1] # center # fontSizeL 0.6
  , drawG g2 # center # fontSizeL 0.5
  ]

main :: IO ()
main = mainWith dia
