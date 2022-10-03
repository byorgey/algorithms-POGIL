#!/usr/bin/env stack
-- stack --resolver lts-19.6 script --package diagrams-lib --package diagrams-pgf --package texrunner --package containers --package random

{-# LANGUAGE ViewPatterns #-}

import Diagrams.Backend.PGF
import Diagrams.Prelude
import GraphDiagrams
import Data.Bifunctor
import System.Random

------------------------------------------------------------

perturb :: P2 Double -> IO (P2 Double)
perturb (coords -> (x :& y)) = do
  dx <- randomRIO (-0.3, 0.3)
  dy <- randomRIO (-0.3, 0.3)
  return ((x + dx) ^& (y + dy))

mvertices :: [(Char, P2 Double)]
mvertices =
  [ ('a', 1 ^& 2)
  , ('b', 0 ^& 1)
  , ('c', 1 ^& 0)
  , ('d', 2 ^& 1)
  , ('i', 3 ^& 0)
  , ('f', 3 ^& 2)
  , ('e', 4 ^& 1)
  , ('j', 4 ^& (-1))
  , ('g', 5 ^& 2)
  , ('h', 5 ^& 0)
  ]

mvertices' :: IO [(Char, P2 Double)]
mvertices' = (traverse . traverse) perturb mvertices

medges =
  [ (('a','b'), 3)
  , (('b','c'), 17)
  , (('b','d'), 16)
  , (('a','f'), 2)
  , (('c','d'), 8)
  , (('c','i'), 18)
  , (('d','i'), 4)
  , (('d','e'), 11)
  , (('i','e'), 10)
  , (('f','e'), 1)
  , (('f','g'), 7)
  , (('e','g'), 6)
  , (('e','h'), 5)
  , (('i','h'), 12)
  , (('i','j'), 9)
  , (('j','h'), 13)
  , (('g','h'), 15)
  ]

g :: IO (Graph Char Int)
g = do
  vs <- mvertices'
  return $ wgraph vs medges

dia1 :: IO (Diagram B)
dia1 = drawGraph (drawVTeX . first (:[])) (drawUWE (tex . show)) <$> g

------------------------------------------------------------

g2 :: Graph Char Int
g2 = wgraph
  [ ('a', 1 ^& 2)
  , ('b', 0 ^& 1)
  , ('c', 2 ^& 1)
  , ('d', 1 ^& 0)
  , ('e', 2 ^& 2)
  ]

  [ (('a','b'), 5)
  , (('a','d'), 9)
  , (('b','d'), 20)
  , (('a','c'), 3)
  , (('c','d'), 1)
  , (('a','e'), 2)
  , (('e','c'), 4)
  ]

gDia = drawGraph (drawVTeX . first (:[])) (drawUWE (tex . show)) g2

dia2 = vsep 1 . map (hsep 1) $
  [[gDia, gDia], [gDia, gDia]]

------------------------------------------------------------

main = do
  d1 <- dia1
  renderPGF "MSTexample.pgf" (mkWidth 300) d1
  renderPGF "MSTtrace.pgf" (mkWidth 400) dia2
