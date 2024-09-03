#!/usr/bin/env stack
-- stack --resolver lts-22.33 script --package diagrams-lib --package diagrams-pgf --package texrunner --package containers --package force-layout

import Data.Char (chr, ord)
import Diagrams.Backend.PGF.CmdLine
import Diagrams.Prelude
import Graphs

-- -w 200
graph1 :: Diagram B
graph1 =
  graph
    [ (0, 1)
    , (1, 2)
    , (2, 0)
    , (3, 4)
    , (4, 5)
    , (6, 6)
    , (7, 7)
    ]
    (\i -> [chr (i + ord 'A')])

-- -w 150
dgraph1 :: Diagram B
dgraph1 =
  graph'
    True
    [(0, 1), (0, 2), (1, 2), (1, 3), (3, 4), (2, 5), (5, 6), (6, 7), (7, 8), (8, 5), (3, 9)]
    (\i -> "$" ++ show i ++ "$")

-- -w 150
dgraph2 :: Diagram B
dgraph2 =
  graph'
    True
    [(0, 2), (1, 2), (2, 3), (3, 4), (4, 5), (3, 6), (6, 7), (6, 8), (6, 9)]
    (\i -> [chr (i + ord 'a')])

-- -w 150
dgraph3 :: Diagram B
dgraph3 =
  graph'
    True
    [ (0, 1)
    , (1, 2)
    , (2, 0)
    , (3, 4)
    , (4, 5)
    , (6, 6)
    , (7, 7)
    ]
    (\i -> [chr (i + ord 'A')])

main :: IO ()
main =
  mainWith
    [ ("graph1", graph1)
    , ("dgraph1", dgraph1)
    , ("dgraph2", dgraph2)
    , ("dgraph3", dgraph3)
    ]
