{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module ReductionGraphs where

import           Diagrams.Backend.PGF.CmdLine
import           Diagrams.Prelude

import Data.Bits
import Data.Char
import Control.Arrow
import qualified Data.Set as S
import qualified Data.Map as M

type Graph v = (M.Map v (P2 Double), S.Set (v, v))

exampleGraph1 :: Graph Int
exampleGraph1
  = ( M.fromList $ [ (1, unitX # rotateBy (1/3))
                   , (2, unitX # rotateBy (1/6))
                   , (3, unit_X)
                   , (4, origin)
                   , (5, unitX)
                   , (6, unitX # rotateBy (2/3))
                   , (7, unitX # rotateBy (5/6))
                   ]
    , S.fromList $ [ (1,2), (1,3), (2,3), (2,4), (2,5)
                   , (3,6), (3,7), (4,7), (5,7), (6,7)
                   ]
    )

exampleGraph2 :: Graph Char
exampleGraph2
  = ( M.fromList $ zip ['a'..] (iterateN numNodes (rotateBy (1/fromIntegral numNodes)) (2 *^ unitX))
    , S.fromList
      [ (toLetter x, toLetter y)
      | x <- [0 .. numNodes - 2]
      , y <- [x+1 .. numNodes - 1]
      , (desc `shiftR` (y*(y-1) `div` 2 + x)) .&. 1 == 1
      ]
    )
  where
    desc = 34009176768908 :: Integer   -- n = 10
--    desc = 32125892104029189430 :: Integer   -- n = 12
--    desc = 159003329 :: Integer    -- n = 8
--    desc = 415166821666407650778300069080260299 :: Integer   -- n = 16
    numNodes = 10
    toLetter i = chr (ord 'a' + i)

drawGraph :: Ord v => (v -> Diagram B) -> Graph v -> Diagram B
drawGraph drawV (nodes, edges) = drawNodes <> drawEdges
  where
    drawNodes = mconcat . map drawNode $ M.assocs nodes
    drawNode (v, p) = drawV v # moveTo p # fontSizeL 0.2
    drawEdges = mconcat . map drawEdge $ S.toList edges
    drawEdge (v1,v2) =
      l1 ~~ l2
        # flip adjust (with & adjSide .~ Both & adjMethod .~ ByAbsolute (-0.35))
        # strokeLocTrail
      where
        l1 = nodes M.! v1
        l2 = nodes M.! v2

tex x = ("$" ++ x ++ "$") # text

-- main = defaultMain (drawFlow False flowExample # frame 1)
