{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude

stop :: Diagram B
stop = mconcat
  [ text "STOP" # fontSizeL 1.2 # font "Sans" # bold # fc white
  , octagon 2 # fc red # lw none # lc red
  ]

main = defaultMain (stop # frame 0.1)
