#!/usr/bin/env stack
-- stack --resolver lts-19.6 script --package diagrams-lib --package diagrams-pgf --package texrunner --package containers --package force-layout

-- Run with -w 150 -h 150

import Diagrams.Prelude
import Diagrams.Backend.PGF.CmdLine
import Data.Char (chr, ord)

import Graphs

conn :: Diagram B
conn = graph
  [(0,1),(0,2),(1,2), (1,3),(3,4),(2,5),(5,6),(6,7),(7,8),(8,5),(3,9)]
  (\i -> "$" ++ show i ++ "$")

tree :: Diagram B
tree = graph
  [(0,2), (1,2), (2,3), (3,4), (4,5), (3,6), (6,7), (6,8), (6,9)]
  (\i -> [chr (i + ord 'a')])

graph3 :: Diagram B
graph3 = graph
  [ (0,1), (1,2), (2,0)
  , (3,4), (4,5)
  , (6,6), (7,7)
  ]
  (\i -> [chr (i + ord 'A')])

main :: IO ()
main = mainWith
  [ ("conn", conn)
  , ("tree", tree)
  , ("graph3", graph3)
  ]
