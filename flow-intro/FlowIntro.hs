{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module FlowIntro where

import           Data.Colour.SRGB
import           Diagrams.Backend.PGF.CmdLine
import           Diagrams.Prelude

import           Control.Arrow
import qualified Data.Map                     as M

type Network = (M.Map Char (P2 Double), M.Map (Char, Char) (Int, Int))

flowExample :: Network
flowExample
  = ( M.fromList $ [ ('s', 0 ^& 0)
                   , ('a', 1 ^& 1)
                   , ('b', 1 ^& 0)
                   , ('c', 1 ^& (-1))
                   , ('d', 2 ^& 1)
                   , ('e', 2 ^& 0)
                   , ('f', 2 ^& (-1))
                   , ('t', 3 ^& 0) ]
    , M.fromList $ [ (('s','a'), 10)
                   , (('s','b'), 5)
                   , (('s','c'), 15)
                   , (('a','b'), 4)
                   , (('b','c'), 4)
                   , (('a','d'), 9)
                   , (('a','e'), 15)
                   , (('b','e'), 8)
                   , (('f','b'), 6)
                   , (('c','f'), 30)
                   , (('d','e'), 15)
                   , (('e','f'), 15)
                   , (('d','t'), 10)
                   , (('e','t'), 10)
                   , (('f','t'), 10)
                   ]
                   # map (second ((,) 0))
    )

withFlow :: M.Map (Char,Char) Int -> Network -> Network
withFlow flow net = foldr (\(e,f) -> second (M.adjust (first (const f)) e)) net (M.assocs flow)

drawFlow :: Bool -> Network -> Diagram B
drawFlow showFlow (nodes, edges) = drawNodes <> drawEdges
  where
    drawNodes = mconcat . map drawNode $ M.assocs nodes
    drawNode (c, p) = tex [c] # moveTo p # fontSizeL 0.2
    drawEdges = mconcat . map drawEdge $ M.assocs edges
    drawEdge ((v1,v2), (f,c)) = mconcat
      [ tex (if showFlow then show f ++ "/" ++ show c else show c)
        # moveTo (lerp 0.5 l1 l2
                  .+^ (if showFlow then 0.15 else 0.1) *^ normalize (perp (l2 .-. l1)))
        # fontSizeL (if showFlow then 0.08 else 0.1)
      , let t :: Located (Trail V2 Double)
            t = l1 ~~ l2 # flip adjust (with & adjSide .~ Both & adjMethod .~ ByAbsolute (-0.35))
            edgeColor | not showFlow = black
                      | f >= c = green -- sRGB24 0x3c 0xbe 0x35
                      | f > 0  = blue  -- sRGB24 0x2f 0xce 0xc8
                      | otherwise = black
            edgeStyle :: Style V2 Double
            edgeStyle | not showFlow = mempty
                      | f >= c = lc edgeColor . lw veryThick $ mempty
                      | f > 0  = lc edgeColor . lw veryThick $ mempty
                      | otherwise = mempty
        in  mconcat
            [ triangle 0.1 # fc edgeColor # lw none # scaleX 0.6
              # rotateBy (1/4)
              # rotateTo (dirBetween l2 l1)
              # moveTo (t # atEnd)
            , t # strokeLocTrail
                # applyStyle edgeStyle
            ]
      ]
      where
        l1 = nodes M.! v1
        l2 = nodes M.! v2
    tex x = ("$" ++ x ++ "$") # text

-- main = defaultMain (drawFlow False flowExample # frame 1)
