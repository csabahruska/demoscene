{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns #-}

import Diagrams.Prelude
import Diagrams.TwoD.Offset--Prelude
import Diagrams.Backend.SVG.CmdLine

import Knot
import AD

-- use in ghci like ':main -o circle.svg -w 600'
main = mainWith (dia :: Diagram B R2)

dia' = fromVertices points
  where
    points = map (toP . project') $ sampleCurve 20 $ unKnot

dia = fromVertices points
  <> mconcat (map (mark red) tpoints)
  where
    points = map (toP . project') $ sampleCurve 140 $ torusKnot 1 7

    tpoints = map (project') $ samplePatch 30 10 $ tubularPatch (mulSV3 2 . unKnot) (mulSV3 0.6 . unKnot)

project' :: V3 Double -> R2
project' (V3 x y z) = r2 (x, y + z / 2)

toP v = origin # translate v

mark c p = circle 0.03 # fc c # lw none # translate p

