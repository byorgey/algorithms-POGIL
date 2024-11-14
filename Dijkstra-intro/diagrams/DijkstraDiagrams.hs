#!/usr/bin/env stack
-- stack --resolver lts-19.6 script --package diagrams-lib --package diagrams-pgf --package texrunner

-- Run with -w 300

import Diagrams.Backend.PGF.CmdLine
import Diagrams.Prelude

import Graphs

g1 :: Graph
g1 =
  Graph
    (zip (map (: []) "sabtc") [(0, 4), (0, 1), (2, 0), (4, 1), (3, 4)])
    [ ("s", "a", Just 3)
    , ("s", "c", Just 9)
    , ("a", "b", Just 2)
    , ("b", "t", Just 4)
    , ("c", "t", Just 2)
    ]

g2 :: Graph
g2 =
  Graph
    (zip (map (: []) "s234567t") [(0, 0), (3, 3), (15, 3), (13, 0), (9, -1), (6, 0), (3, -3), (15, -3)])
    [ ("s", "d", Just 9)
    , ("s", "i", Just 15)
    , ("s", "h", Just 14)
    , ("h", "i", Just 5)
    , ("d", "e", Just 24)
    , ("h", "e", Just 18)
    , ("h", "g", Just 30)
    , ("g", "e", Just 2)
    , ("g", "f", Just 11)
    , ("f", "e", Just 6)
    , ("f", "t", Just 6)
    , ("e", "t", Just 19)
    , ("i", "t", Just 44)
    , ("i", "g", Just 20)
    , ("g", "t", Just 16)
    ]

dia :: Diagram B
dia =
  vsep
    5
    [ hsep 2 [drawG (unweight g1), drawG g1] # center # fontSizeL 0.6
    , drawG g2 # center # fontSizeL 0.5
    ]

main :: IO ()
main = mainWith dia
