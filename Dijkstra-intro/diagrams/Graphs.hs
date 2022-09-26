module Graphs where

import           Diagrams.Backend.PGF
import           Diagrams.Prelude

data Graph = Graph [(String, (Double, Double))] [(String, String, Maybe Int)]

drawG :: Graph -> Diagram B
drawG (Graph vs es) =
  map (\(v, l) -> node # named v # moveTo (p2 l)) vs
  # mconcat
  # applyAll
    [ useLabel v v | (v,_) <- vs ]
  # applyAll
    [ labelWeight i j wgt . connectOutside' opts i j | (i,j,wgt) <- es ]

opts = with & headLength .~ local 0.4

unweight :: Graph -> Graph
unweight (Graph vs es) = Graph vs (map (\(i,j,_) -> (i,j,Nothing)) es)

useLabel :: IsName n => n -> String -> (Diagram B -> Diagram B)
useLabel n l = sideLabel n l zero

sideLabel :: IsName n => n -> String -> V2 Double -> (Diagram B -> Diagram B)
sideLabel i l v = withName i $ \sub -> -- $
  atop (text ("$" ++ l ++ "$") # moveTo (location sub) # translate v)

labelWeight :: IsName n => n -> n -> Maybe Int -> (Diagram B -> Diagram B)
labelWeight _ _ Nothing  = id
labelWeight i j (Just w) = withNames [i,j] $ \[s1, s2] ->  -- $
  atop (text ("$" ++ show w ++ "$")
          # moveTo (lerp 0.5 (location s1) (location s2))
          # translate (0.4 *^ normalize (perp (location s2 .-. location s1)))
  )

pip, node :: Diagram B
pip  = circle 0.2 # fc black
node = circle 0.5
