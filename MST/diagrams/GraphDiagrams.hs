{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}

module GraphDiagrams where

import           Diagrams.Backend.PGF.CmdLine
import           Diagrams.Prelude

import           Control.Arrow
import           Data.Bits
import           Data.Char
import           Data.Map                     (Map, (!))
import qualified Data.Map                     as M
import qualified Data.Set                     as S

data Graph v w = Graph
  { vertices :: Map v (P2 Double)
  , edges    :: S.Set ((v, v), w)
  }

exampleGraph1 :: Graph Int ()
exampleGraph1 = ugraph

  [ (1, unitX # rotateBy (1/3))
  , (2, unitX # rotateBy (1/6))
  , (3, unit_X)
  , (4, origin)
  , (5, unitX)
  , (6, unitX # rotateBy (2/3))
  , (7, unitX # rotateBy (5/6))
  ]

  [ (1,2), (1,3), (2,3), (2,4), (2,5)
  , (3,6), (3,7), (4,7), (5,7), (6,7)
  ]

ugraph :: Ord v => [(v, P2 Double)] -> [(v,v)] -> Graph v ()
ugraph vs es = wgraph vs (map (,()) es)

wgraph :: (Ord v, Ord w) => [(v, P2 Double)] -> [((v,v),w)] -> Graph v w
wgraph vs es = Graph (M.fromList vs) (S.fromList es)

drawGraph :: Ord v => ((v, P2 Double) -> Diagram B) -> ((v, P2 Double) -> (v, P2 Double) -> w -> Diagram B) -> Graph v w -> Diagram B
drawGraph drawV drawE (Graph{..}) = drawNodes <> drawEdges
  where
    drawNodes = mconcat . map drawV $ M.assocs vertices
    drawEdges = mconcat . map drawE' $ S.toList edges
    drawE' ((v1, v2), w) = drawE (v1, vertices ! v1) (v2, vertices ! v2) w

tex x = ("$" ++ x ++ "$") # text # fontSizeL 0.15

drawVTeX (v, p) = tex v # moveTo p

drawUUE :: (v, P2 Double) -> (v, P2 Double) -> w -> Diagram B
drawUUE (_,p1) (_,p2) _ =
  p1 ~~ p2
    # flip adjust (with & adjSide .~ Both & adjMethod .~ ByAbsolute (-0.35))
    # strokeLocTrail

drawUWE :: (w -> Diagram B) -> (v, P2 Double) -> (v, P2 Double) -> w -> Diagram B
drawUWE drawW v1@(_,p1) v2@(_,p2) w = mconcat
  [ drawUUE v1 v2 w
  , drawW w # moveTo (lerp 0.5 p1 p2)
            # translate (0.2 *^ normalize (perp (p2 .-. p1)))
  ]

-- main = defaultMain (drawFlow False flowExample # frame 1)
