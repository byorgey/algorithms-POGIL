#!/usr/bin/env stack
-- stack --resolver lts-24.20 script --package diagrams-lib --package diagrams-pgf --package texrunner --package colour --package containers

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module BinomialHeaps where

import Data.Colour.SRGB
import Data.List (intersperse)
import Data.Tree
import Diagrams.Backend.PGF.CmdLine
import Diagrams.Prelude

drawLeftLeaningTree :: (a -> Diagram B) -> Tree a -> Diagram B
drawLeftLeaningTree node (Node a ts) =
  vsep
    2
    [ node a # named "root"
    , hsep 2 (zipWith (\k t -> drawLeftLeaningTree node t # named k) [0 :: Int ..] ts)
    ]
    # applyAll [connectOutside' connOpts "root" k | k <- [0 .. length ts - 1]]
    # localize
 where
  connOpts = with & arrowHead .~ noHead

binomialTreeShape :: Int -> Tree ()
binomialTreeShape 0 = Node () []
binomialTreeShape n = Node () (map binomialTreeShape [0 .. n - 1])

-- binomialTree :: Int -> Diagram B
-- binomialTree 0 = circle 1
-- binomialTree n =
--   vsep 2
--   [ circle 1 # named "root"
--   , cat' unit_X (with & sep .~ 2) [binomialTree k # named k | k <- [0 .. n-1]]
--   ]
--   # applyAll [connectOutside' connOpts "root" k | k <- [0 .. n-1]]
--   # localize
--   where
--     connOpts = with & arrowHead .~ noHead

drawBinomialTreeShape :: Int -> Diagram B
drawBinomialTreeShape = drawLeftLeaningTree (const (circle 1)) . binomialTreeShape

binomialTrees :: Diagram B
binomialTrees = hsep 4 [drawBinomialTreeShape n | n <- [0 .. 4]]

drawBinomialHeap :: (a -> Diagram B) -> [Tree a] -> Diagram B
drawBinomialHeap node = hsep 4 . map (drawLeftLeaningTree node)

bt1 x = Node 12 [Node 77 [], Node 13 [Node 28 []], Node x [Node 24 [], Node 33 [Node 53 []]]]
bt2 = Node 5 [Node 21 [], Node 17 [Node 99 []]]

goodBinomialHeap :: [Tree Int]
goodBinomialHeap = [bt1 23, bt2, Node 8 []]

badBinomialHeap1 :: [Tree Int]
badBinomialHeap1 = [bt1 25, bt2, Node 8 []]

badBinomialHeap2 :: [Tree Int]
badBinomialHeap2 = [bt1 23, Node 4 [], bt2, Node 8 []]

binomialHeaps :: Diagram B
binomialHeaps =
  [badBinomialHeap1, goodBinomialHeap, badBinomialHeap2]
    # map (drawBinomialHeap drawNode)
    # intersperse (hrule 50 # lc gray)
    # map centerX
    # vsep 2

addend :: [Tree Int]
addend = [Node 2 [Node 19 []], Node 3 []]

heapMerge :: Diagram B
heapMerge = vsep 3 (map (drawBinomialHeap drawNode) [goodBinomialHeap, addend])

drawNode :: Int -> Diagram B
drawNode = fontSizeL 0.7 . (<> circle 1) . text . (++ "$") . ("$" ++) . show

main = do
  renderPGF "binomial-trees.pgf" (mkWidth 400) binomialTrees
  renderPGF "binomial-heaps.pgf" (mkWidth 300) binomialHeaps
  renderPGF "heap-merge.pgf" (mkWidth 400) heapMerge
