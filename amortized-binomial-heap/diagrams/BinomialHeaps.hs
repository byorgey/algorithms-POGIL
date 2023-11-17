#!/usr/bin/env stack
-- stack --resolver lts-19.6 script --package diagrams-lib --package diagrams-pgf --package texrunner --package colour --package containers

{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module BinomialHeaps where

import           Data.Colour.SRGB
import           Diagrams.Backend.PGF.CmdLine
import           Diagrams.Prelude

binomialTree :: Int -> Diagram B
binomialTree 0 = circle 1
binomialTree n =
  vsep 2
  [ circle 1 # named "root"
  , cat' unit_X (with & sep .~ 2) [binomialTree k # named k | k <- [0 .. n-1]]
  ]
  # applyAll [connectOutside' connOpts "root" k | k <- [0 .. n-1]]
  # localize
  where
    connOpts = with & arrowHead .~ noHead

binomialTrees :: Diagram B
binomialTrees = hsep 3 [binomialTree n | n <- [0 .. 4]]

main = do
  renderPGF "binomial-trees.pgf" (mkWidth 400) binomialTrees
