{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Graphs where

import           Diagrams.Prelude

import           Diagrams.Backend.PGF

import           Data.Bifunctor       (second)
import           Data.List            (maximumBy, nub, sort)
import qualified Data.Map             as M
import           Data.Ord             (comparing)
import           Physics.ForceLayout

graph :: _ => [(Int,Int)] -> (Int -> String) -> Diagram PGF
graph es vLabel = drawEnsemble es vLabel $
  forceLayout
    ( with
    & damping     .~ 0.8
    & energyLimit .~ Just 0.001
    & stepLimit   .~ Nothing
    )
  ens

  where
    ens :: Ensemble V2 Double
    ens = Ensemble
      [ (es,  hookeForce 0.05 4)
      , (allPairs, coulombForce 1)
      ]
      particleMap
    vs = nub (map fst es ++ map snd es)
    allPairs = [(x,y) | x <- vs, y <- vs, x < y ]
    particleMap :: M.Map Int (Particle V2 Double)
    particleMap = M.fromList $ zip vs (map initParticle (regPoly (length vs) 4))

drawEnsemble :: _ => [(Int,Int)] -> (Int -> String) -> Ensemble V2 Double -> Diagram PGF
drawEnsemble es vLabel ens
  = applyAll (map drawEdge es) . mconcat . map drawPt $ ps
  where
    drawPt (pid, p) = mconcat
      [ pip # named pid # moveTo p
      , text (vLabel pid)
        # fontSizeO 8
        # moveTo (p ^+^ (unitX # rotate bestAngle))
      ]
      where
        edgeAngles = es
          # filter (\(x,y) -> x == pid || y == pid)
          # map (\(x,y) -> if x == pid then y else x)
          # map (\x -> (((pmap M.! x) ^. pos) .-. p) ^. _theta)
        bestAngle = case sort edgeAngles of
          [a] -> a ^+^ (180 @@ deg)
          srt -> zipWith (\a1 a2 -> (a2 ^-^ a1, lerp 0.5 a1 a2))
                   srt
                   (tail srt ++ [head srt ^+^ (1 @@ turn)])
            # maximumBy (comparing fst)
            # snd
    pmap = ens ^. particles
    ps = (map . second) (view pos) . M.assocs $ pmap
    drawEdge (v1,v2) = withNames [v1,v2] $ \[s1,s2] -> beneath (location s1 ~~ location s2)

pip :: _ => Diagram PGF
pip = circle 0.2 # fc black
