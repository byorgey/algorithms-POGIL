#!/usr/bin/env stack
-- stack --resolver lts-19.6 script --package diagrams-lib --package diagrams-contrib --package diagrams-pgf --package texrunner --package containers --package random

{-# LANGUAGE ViewPatterns #-}

import Diagrams.Backend.PGF
import Diagrams.TwoD.Layout.Tree
import Diagrams.Prelude hiding (Empty)

import Data.Maybe

mergeSortTree :: Int -> BTree String
mergeSortTree = go 1
  where
    go _ 0 = Empty
    go k n = BNode ("n" ++ if (k > 1) then ("/" ++ show k) else "") s s
      where s = go (2*k) (n-1)

tree :: Diagram B
tree = maybe mempty
        (renderTree (\l -> text ("$" ++ l ++ "$") <> square 2 # lw none # fc white ) (~~))
    . symmLayoutBin' (with & slHSep .~ 4 & slVSep .~ 3)
    $ mergeSortTree 4   -- $

dia :: Diagram B
dia = vsep 1 [tree, vsep 0.3 (replicate 3 (circle 0.2 # fc black))]


main = renderPGF "merge-tree.pgf" (mkWidth 300) dia
