{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
module Program where

--import Data.Maybe
import Control.Applicative hiding (Const)
import Control.Monad.Identity

import Knot
import AD
import Data.Vect
import Utility
import KnotsLC

--import LambdaCube.GL hiding (Exp, Var, Let, V3, V2)
import qualified LambdaCube.GL as LC

---------------------

wires :: IO (Wire Int ExpV1)
wires = program $ WHorizontal
    [ WVertical
        [ (wire1D 200 $ mulSV3 (sin (3* time) + 1.1) . unKnot) {wDuration = Just 3}
        , wire1D 200 $ mulSV3 1.1 . unKnot
        ]
    , WSound Nothing "music/Take_Them.ogg"
    , WVertical
      [ wText2D (Just 2) defaultCam ""
      , WHorizontal
        [ WVertical [ delay 0, wText2D (Just 6) defaultCam "lambda presents" ]
        , WVertical [ delay 8, wText2D (Just 8) defaultCam "\nknot theory" ]
        ]
      , WHorizontal
        [ WVertical [ delay 0, wText2D (Just 8) defaultCam "who do you think" ]
        , WVertical [ delay 3, wText2D (Just 6) defaultCam "\nis going to win" ]
        , WVertical [ delay 6, wText2D (Just 4) defaultCam "\n\ncapitalism" ]
        ]
      , WHorizontal
        [ WVertical [ delay 0, wText2D (Just 8) defaultCam "what is keeping us" ]
        , WVertical [ delay 3, wText2D (Just 6) defaultCam "\nfrom embracing" ]
        , WVertical [ delay 6, wText2D (Just 4) defaultCam "\n\nour brighter future" ]
        ]
      , WHorizontal
        [ WVertical [ delay 0, wText2D (Just 8) defaultCam "with humans gone" ]
        , WVertical [ delay 3, wText2D (Just 6) defaultCam "\nwill there be hope" ]
        , WVertical [ delay 6, wText2D (Just 4) defaultCam "\n\nfor machines" ]
        ]
      , WHorizontal
        [ WVertical [ delay 0, wText2D (Just 8) defaultCam "greetings:" ]
        , WVertical [ wText2D (Just 1) defaultCam ("\n  " ++ name) | name <- greetingNames ]
        ]
      , WHorizontal
        [ WVertical [ delay 0, wText2D (Just 4) defaultCam "music:" ]
        , WVertical [ delay 1, wText2D (Just 3) defaultCam "\n  ficture" ]
        ]
      , WHorizontal
        [ WVertical [ delay 0, wText2D (Just 4) defaultCam "code:" ]
        , WVertical [ delay 1, wText2D (Just 3) defaultCam "\n  divip\n  hcs\n  hranolky" ]
        ]
      , WHorizontal
        [ WVertical [ delay 0, wText2D (Just 8) defaultCam "with machines gone" ]
        , WVertical [ delay 3, wText2D (Just 6) defaultCam "\nwill there be hope" ]
        , WVertical [ delay 6, wText2D (Just 4) defaultCam "\n\nfor humans" ]
        ]
      , WFadeOut (Just 5)
      ]
    , wire2DNorm False 60 16 $ tubularPatch (mulSV3 2 . unKnot) (mulSV3 (0.1 * (sin (4 * time) + 5)) . unKnot)
    , (wire2DNormAlpha True 2000 3 (tubularNeighbourhood (helix 2 0) . translateZ (0.2 * sin (6 * time)) . twistZ 1 . magnifyZ 50 . magnifyX 0.2 . translateY 0.65 . translateX (-0.5) . planeZX) (Just $ \(V2 _ y) -> V3 y 0.5 0.5) Nothing) {wSimpleColor = True}
--    wire2DNorm False 200 20 $ magnifyZ 3 . cylinderZ 0.3
--    wire2DNorm False 200 20 $ twistZ 1 . translateX 0.5 . magnifyZ 3 . cylinderZ 0.1
--    wire1D 100 $ translateZ (-1.5) . helix 0.3 0.5 . (10 *)
--    wire1D 1000 $ translateZ (-1.5) . tubularNeighbourhood (helix 0.3 0.5) . helix 0.1 (0.5/3) . (50*)
--    wire2DNorm False 100 10 $ translateZ (-1.5) . tubularNeighbourhood (helix 0.3 0.5) . cylinderZ 0.08 . (10*)
--    wire1D 10000 $ env . helix (0.1/3) (0.5/9) . (200 *)
--    wire2DNorm False 1000 10 $ env . cylinderZ 0.015 . (50*)

    , WVertical
        [
        ---------
          WCamera (Just  9) $ CamCurve $ mulSV3 2 . unKnot
        , WCamera (Just  9) $ CamCurve $ (V3 0 0 2 +) . mulSV3 2 . unKnot
        , WCamera (Just  8) $ CamCurve $ magnify 0.5 . lissajousKnot (V3 3 2 5) (V3 0.7 0.1 0)
        , WCamera (Just  5) $ CamMat $ fromProjective (lookat (Vec3 4 3 3) (Vec3 0 0 0) (Vec3 0 1 0))
        , WCamera (Just 10) $ CamCurve $ torusKnot 7 3
        , WCamera (Just 10) $ CamMat $ fromProjective (lookat (Vec3 0 3 1) (Vec3 0 1 0) (Vec3 0 1 0))
        , WCamera (Just 10) $ CamMat $ fromProjective (lookat (Vec3 0 0 2) (Vec3 7 0 9) (Vec3 0 1 0))
        ---------
        , WCamera Nothing $ CamMat $ fromProjective (lookat (Vec3 5 1 2) (Vec3 3 1 0) (Vec3 0 1 0))
        ]


    , wParticle 10 10 10 (magnify 20 . hopf . translateY 0.1 . rotateYZ 0.1 . translateX 0.1 . rotateXZ 3 . magnify (2*pi)) Nothing

    , WVertical
        [ transW env3 $ WHorizontal
            [ delay 30
            , cyl 10000 (0.1, 0.2, 200)
            , wire2DNormAlpha True 1000 10 (magnifyZ 60 . rotateXY time . twistZ 1 . translateY (-0.5) . planeZY)
                                (Just $ \(V2 x y) -> V3 x 1 y) (Just $ \(V2 _ y) -> y)
            ]
        , transW (middleSin $ archimedeanSpiralN 0.02 0) $ WHorizontal
            [ delay 10
            , cyl 10000 (0.1, 0.2, 200)
            , wire2DNorm False 1000 10 $ cylinderZ 0.08 . (70*)
            ]
        , transW env3 $ WHorizontal
            [ delay 10
            , wire2DNorm False 1000 10 $ cylinderZ 0.08 . (60*)
            , wire2DNorm True 1000 10 $ translateY (-0.5) . magnifyZ 60 . planeZY
            ]
        , transW (magnify 2 . tubularNeighbourhood (helix 0.9 (sin time + 1.5)) . tubularNeighbourhood (helix 0.3 0.5 . (+ 0.5 * sin (2 * time))) . tubularNeighbourhood (helix 0.1 (0.5/3) . (+ 0.03 * sin (10 * time)))) $ WHorizontal
            [ delay 20
            , cyl 20000 (0.1/3, 0.5/9, 200)
            , wire2DNorm False 2400 10 $ cylinderZ 0.015 . (50*)
            ]

{-
        , WHorizontal
            [ wire2DNorm False 200 20 $ magnifyZ 3 . cylinderZ 0.3
            , wire2DNorm False 200 20 $ twistZ 1 . translateX 0.5 . magnifyZ 3 . cylinderZ 0.1
            ]
        , WHorizontal
            [ wire1D 100 $ translateZ (-1.5) . helix 0.3 0.5 . (10 *)
            ]

        , WHorizontal
            [ wire1D 1000 $ translateZ (-1.5) . tubularNeighbourhood (helix 0.3 0.5) . helix 0.1 (0.5/3) . (50*)
            , wire2DNorm False 100 10 $ translateZ (-1.5) . tubularNeighbourhood (helix 0.3 0.5) . cylinderZ 0.08 . (10*)
            ]
-}
        ]
--    wire2DNorm False 1000 10 $ env3 . cylinderZ 0.08 . (60*)
--    wire2DNorm True 1000 10 $ env3 . translateY (-0.5) . magnifyZ 60 . planeZY
--    wire2DNorm True 2 2 $ planeXY
--    wire2DNorm False 200 20 $ twistZ 1 . translateX 0.5 . magnifyZ 3 . cylinderZ 0.1
--    wire2DNorm False 50 50 $ magnifyZ 100 . projectionZ . magnifyZ 0.01 . invPolarXY . magnify (2 * pi) . translateX (-1) . planeZY
--    wire2DNorm False 50 50 $
--        magnify 100 . projectionZ . magnify 0.01 . invPolarXY . rotateYZ (- pi / 4) . magnify (8 * pi) . translateX (-2) . magnifyZ 0.1 .
--        magnify 100 . projectionZ . magnify 0.01 . invPolarXY . magnify (2 * pi) . translateX (-1) .
--        planeZY

    , wire2DNormAlpha False 500 10 (tubularPatch (mulSV3 3 . lissajousKnot (V3 3 4 7) (V3 0.1 0.7 0.0)) (mulSV3 0.1 . unKnot))
                (Just $ const $ V3 1 1 1) (Just $ const $ 0.5)
--    wire2DNormAlpha True 20 20 (magnify 3 . translateY (-0.5) . planeYZ) (Just $ sin . normV2)
    ]
  where
    cyl :: Int -> (forall t . Timed t => (t, t, t)) -> Wire_ (Maybe x) () Exp
--    cyl n (a, b, c) = wire1D n $ helix a b . (c *)
    cyl n tup = (wire2DNormAlpha True n 3 (f tup) (Just $ \(V2 x _) -> V3 1 0.9 (0.5 * sin (2 * c * x) + 0.5)) Nothing) {wSimpleColor = True}
      where
        f (a, b, c) = twistZ (helixTurn a b / helixHeight a b) . magnifyZ (helixHeight a b * c) . translateY a . magnifyX 0.01 . translateX (-0.5) . planeZX
--          where
--            turn = c * helixTurn a b

    middleSin :: Curve Identity -> SpaceTr Identity
    middleSin c = magnify 1.5 . tubularNeighbourhood (liftA2 (+) id ((\t -> V3 0 0 t) . (/15) . sin . (*6) . (+ (0.5 * time)) . normV3) . c)
    env3 :: SpaceTr Identity
    env3 = middleSin $ logarithmicSpiral 0.1 0.04

defaultCam :: LC.M33F
defaultCam = LC.V3 (LC.V3 (scale * 0.75) 0 0) (LC.V3 0 scale 0) (LC.V3 ofsX ofsY 1)
  where
    scale = 0.1
    ofsX = -0.3
    ofsY = 0

greetingNames =
  [ ""
  , "s3c"
  , "archee"
  , "alvaro"
  , "shr"
  , "rrrola"
  , "a moiré misszió\n  bemutatja"
  ]


---------------------

wiresTest :: IO (Wire Int ExpV1)
wiresTest = program $ WHorizontal
  [ delay 100
  , wire1D 200 $ mulSV3 (sin (3* time) + 1.1) . unKnot
  , wire2DNorm False 60 16 $ tubularPatch (mulSV3 2 . unKnot) (mulSV3 (0.1 * (sin (4 * time) + 5)) . unKnot)
  , (wire2DNormAlpha True 1000 5 (tubularNeighbourhood (helix 2 0) . translateZ (0.2 * sin (6 * time)) . twistZ 1 . magnifyZ 50 . magnifyX 0.2 . translateY 0.65 . translateX (-0.5) . planeZX) (Just $ const $ V3 0.5 0.5 0.5) Nothing) {wSimpleColor = True}
  ]


