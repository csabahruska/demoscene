{-# LANGUAGE OverloadedStrings, PackageImports, TypeOperators, DataKinds #-}

import qualified Graphics.UI.GLFW as GLFW
import Control.Applicative hiding (Const)
import Control.Monad
import Data.Word
import Data.ByteString.Char8 (ByteString)
import Data.IORef
import Data.Vect
import Data.Vect.Float.Instances ()
import FRP.Elerea.Param
import qualified Data.ByteString.Char8 as SB
import qualified Data.Trie as T
import qualified Data.Vector.Storable as V

import LambdaCube.GL

import Graphics.Rendering.OpenGL.Raw.Core32
import LambdaCube.GL.Mesh

--import qualified Criterion.Measurement as C

import VSM
import Utility
import Utils

import Math.Noise hiding (zero)
import Math.Noise.Modules.Billow
import Data.Maybe
import Data.Bitmap.Pure


quad :: Mesh
quad = Mesh
    { mAttributes   = T.singleton "position" $ A_V2F $ V.fromList [V2 a b, V2 a a, V2 b a, V2 b a, V2 b b, V2 a b]
    , mPrimitive    = P_Triangles
    , mGPUData      = Nothing
    }
  where
    a = -1
    b = 1

post :: Exp Obj (Image 1 V4F) -> Exp Obj (FrameBuffer 1 (Float,V4F))
post img = Accumulate fragCtx PassAll frag rast clear
  where
    fragCtx = AccumulationContext Nothing $ DepthOp Always False:.ColorOp NoBlending (one' :: V4B):.ZT
    clear   = FrameBuffer (DepthImage n1 1000:.ColorImage n1 (V4 1 0 0 1):.ZT)
    rast    = Rasterize triangleCtx prims
    prims   = Transform vert input
    input   = Fetch "postSlot" Triangles (IV2F "position")
    --input = FetchData Triangle (AV2F pos)
    --pos   = V.fromList [V2 a b, V2 a a, V2 b a, V2 b a, V2 b b, V2 a b]

    vert :: Exp V V2F -> VertexOut () V2F
    vert uv = VertexOut v4 (Const 1) ZT (NoPerspective uv:.ZT)
      where
        v4      = pack' $ V4 u v (floatV 1) (floatV 1)
        V2 u v  = unpack' uv

    frag :: Exp F V2F -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    frag uv' = FragmentOutRastDepth $ (s{- @+ c-}) :. ZT
      where
        s :: Exp F V4F
        s = texture' smp uv
        --s = texture' smp (Const $ V2 0.8 0.8)
        c = pack' $ V4 (floatF 0) u v (floatF 1)
        V2 u v = unpack' uv
        uv = uv' @* floatF 0.5 @+ floatF 0.5

    smp = Sampler LinearFilter ClampToEdge tex
    tex = Texture (Texture2D (Float RGBA) n1) (V2 512 512) NoMip [img]

screenQuad :: Exp Obj (Image 1 V4F) -> Exp Obj (FrameBuffer 1 V4F)
screenQuad img = Accumulate fragCtx PassAll frag rast clear
  where
    fragCtx = AccumulationContext Nothing $ ColorOp NoBlending (one' :: V4B):.ZT
    clear   = FrameBuffer (ColorImage n1 (V4 1 0 0 1):.ZT)
    rast    = Rasterize triangleCtx prims
    prims   = Transform vert input
    input   = Fetch "postSlot" Triangles (IV2F "position")

    vert :: Exp V V2F -> VertexOut () V2F
    vert uv = VertexOut v4 (Const 1) ZT (NoPerspective uv:.ZT)
      where
        v4      = pack' $ V4 u v (floatV 1) (floatV 1)
        V2 u v  = unpack' uv

    up = Uni (IFloat "up")
    down = Uni (IFloat "down")
    time = Uni (IFloat "time")
    frag :: Exp F V2F -> FragmentOut (Color V4F :+: ZZ)
    frag uv' = FragmentOut $ c :. ZT
      where
        c = Cond (down @< r @&& r @< up) (mask @+ (texel2 @* floatF 0.3)) $
            Cond (down @< r) (pack' $ V4 tR tG tB (floatF 1)) texel2
        texel = smp "ScreenQuad" uv
        texel2 = smp' img uv
        {-
        fnUV v = smp' img (uv @+ rot @* off @+ v @* floatF 0.3)
        V3 oR oG oB = unpack' $ (noise3' uv :: Exp F V3F)
        V4 tR _ _ _ = unpack' $ fnUV oR
        V4 _ tG _ _ = unpack' $ fnUV oG
        V4 _ _ tB _ = unpack' $ fnUV oB
        -}
        V4 tR tG tB _ = unpack' $ smp' img (uv @+ rot @* off)
        mask = pack' $ V4 (floatF 0.1) (floatF 0.5) (floatF 1) (floatF 1)
        V4 r' g b a = unpack' texel
        r = g
        V2 u v = unpack' uv
        uv = uv' @* floatF 0.5 @+ floatF 0.5
        off = (pack' $ V2 r (r @* r)) @* (floatF 0.1 @+ floatF 0.32 @* (sin' (time @* floatF 6) :: Exp F Float))
        t = time @* floatF 10
        rot = pack' $ V2 (fract' (t @* floatF 0.1)) (floatF 0) :: Exp F V2F
        smp n uv = texture' (Sampler LinearFilter ClampToEdge $ TextureSlot n $ Texture2D (Float RGBA) n1) uv
        smp' i uv = texture' (Sampler LinearFilter ClampToEdge $ Texture (Texture2D (Float RGBA) n1) (V2 512 512) NoMip [i]) uv

main :: IO ()
main = do
    let lcnet :: Exp Obj (Image 1 V4F)
        --lcnet = PrjFrameBuffer "outFB" tix0 $ moments
        --lcnet = PrjFrameBuffer "outFB" tix0 vsm
        lcnet = PrjFrameBuffer "outFB" tix0 $ screenQuad $ PrjFrameBuffer "outFB" tix0 vsm
        --lcnet = PrjFrameBuffer "outFB" tix0 $ post $ PrjFrameBuffer "post" tix0 vsm
        --lcnet = PrjFrameBuffer "outFB" tix0 $ post $ PrjFrameBuffer "post" tix0 (blurVH $ PrjFrameBuffer "" tix0 vsm)
        --lcnet = PrjFrameBuffer "outFB" tix0 $ post $ PrjFrameBuffer "post" tix0 $ FrameBuffer (V2 0 0) (DepthImage n1 0:.ColorImage n1 (V4 0 0 1 1 :: V4F):.ZT)

    (win, windowSize) <- initWindow "LC DSL Texture Demo" 512 512

    renderer <- compileRenderer $ ScreenOut lcnet
    print $ slotUniform renderer
    print $ slotStream renderer
    print "renderer created"

    (mousePosition,mousePositionSink) <- external (0,0)
    (fblrPress,fblrPressSink) <- external (False,False,False,False,False)

    compiledQuad <- compileMesh quad
    addMesh renderer "postSlot" compiledQuad []

    mesh <- loadMesh "models/Monkey.lcmesh"
    mesh2 <- loadMesh "models/Scene.lcmesh"

    obj <- addMesh renderer "streamSlot" mesh []
    obj2 <- addMesh renderer "streamSlot" mesh2 []

--    addMesh renderer "streamSlot1" mesh []
--    addMesh renderer "streamSlot1" mesh2 []

    -- TODO:
    -- texture specification:
    --  create buffer with image data
    --  create Texture type from uploaded image data
    --      idea: we should reuse TexSizeRepr for data specification
    --  set sampler uniform to required texture
{-
  alternative A:
    pixelBuffer <- compileBuffer $
        [ Array ArrWord8 (3 * width * height) imagePixelData0
        , Array ArrWord8 (3 * width * height) imagePixelData1
        ]
    texture <- compileTexture $ TextureData (Texture2D (Float RGB) n1) (V2 128 128) Mip $
        [ ImageData pixelBuffer 0 -- mip levels
        , ImageData pixelBuffer 1
        ]
  alternative B:
    texture <- compileTexture $ TextureData (Texture2D (Float RGB) n1) (V2 128 128) Mip $
        [ Array ArrWord8 (3 * width * height) imagePixelData0
        , Array ArrWord8 (3 * width * height) imagePixelData1
        ]
  alternative C:
    texture <- compileTexture $ TextureData (Texture2D (Float RGB) n1) (V2 128 128) Mip
    updateTexture texture $
        [ Array ArrWord8 (3 * width * height) imagePixelData0
        , Array ArrWord8 (3 * width * height) imagePixelData1
        ]
-}

    let objU  = objectUniformSetter obj
        slotU = uniformSetter renderer
        draw _ = do
            render renderer
            GLFW.swapBuffers win
            GLFW.pollEvents
            --putStrLn $ C.secs t ++ " - render frame"
            return ()
        diffuse = uniformFTexture2D "ScreenQuad" slotU

    let p   = perlin
        clamp :: Double -> Word8
        clamp = floor . max 0 . min 255
        calc noiseF w h i j = (\v ->  (v + 1.0) * 127.5 ) $ noiseClampedVal
          where
            boundBottomX :: Double
            boundBottomX = 0.0
            boundBottomY :: Double
            boundBottomY = 0.0
            boundUpperX :: Double
            boundUpperX = 10.0
            boundUpperY :: Double
            boundUpperY = 10.0
            xsize = w
            ysize = h
            xIncrement :: Double
            xIncrement = (boundUpperX - boundBottomX) / (fromIntegral xsize)
            yIncrement :: Double
            yIncrement = (boundUpperY - boundBottomY) / (fromIntegral ysize)
            xPos x = ((fromIntegral x) * xIncrement)  +  boundBottomX
            yPos y = ((fromIntegral y) * yIncrement)  +  boundBottomY

            --noiseF :: NoiseModule
            --noiseF = gen perlin { perlinFrequency = 0.6, perlinOctaves = 5, perlinSeed = seed }
            --noiseF = gen billow { billowFrequency = 0.6, billowOctaves = 5 }

            -- Actual noise computation, getValue returns Maybe Double
            noiseValue = fromMaybe (-1.0) $ getValue noiseF (xPos i, yPos j, 2.123)
            -- Make sure the noiseValue is in the [-1.0, 1.0] range
            noiseClampedVal = if noiseValue > 1.0 
                                 then 1.0
                                 else if noiseValue < (-1.0) then (-1.0)
                                                             else noiseValue
        
        ch1 = createSingleChannelBitmap (512,512) Nothing (\i j -> clamp $
            calc (gen perlin { perlinFrequency = 0.6, perlinOctaves = 5, perlinSeed = 123 }) 512 512 i j)
        ch2 = createSingleChannelBitmap (512,512) Nothing (\i j -> clamp $
            calc (gen perlin { perlinFrequency = 1.1, perlinOctaves = 9, perlinSeed = 123 }) 512 512 i j)
        ch3 = createSingleChannelBitmap (512,512) Nothing (\i j -> clamp $
            calc (gen perlin { perlinFrequency = 0.6, perlinOctaves = 5, perlinSeed = 125 }) 512 512 i j)
        img = combineChannels [ch1,ch2,ch3] Nothing

    diffuse =<< compileTexture2DRGBAF False True img

    s <- fpsState
    sc <- start $ do
        u <- scene (setScreenSize renderer) slotU objU windowSize mousePosition fblrPress
        return $ draw <$> u
    driveNetwork sc (readInput win s mousePositionSink fblrPressSink)

    dispose renderer
    print "renderer destroyed"
    GLFW.destroyWindow win
    GLFW.terminate

scene :: (Word -> Word -> IO ())
      -> T.Trie InputSetter
      -> T.Trie InputSetter
      -> Signal (Int, Int)
      -> Signal (Float, Float)
      -> Signal (Bool, Bool, Bool, Bool, Bool)
      -> SignalGen Float (Signal ())
scene setSize slotU objU windowSize mousePosition fblrPress = do
    time <- stateful 0 (+)
    last2 <- transfer ((0,0),(0,0)) (\_ n (_,b) -> (b,n)) mousePosition
    let mouseMove = (\((ox,oy),(nx,ny)) -> (nx-ox,ny-oy)) <$> last2
    cam <- userCamera (Vec3 (-4) 0 0) mouseMove fblrPress
    let matSetter       = uniformM44F "worldViewProj" slotU
        lightSetter     = uniformM44F "lightViewProj" slotU
        lightSetter2    = uniformM44F "lightViewProj2" slotU
        timeSetter      = uniformFloat "time" slotU
        scaleU          = uniformFloat "scaleU" slotU
        scaleV          = uniformFloat "scaleV" slotU
        upU             = uniformFloat "up" slotU
        downU           = uniformFloat "down" slotU
        setupGFX (w,h) (cam,dir,up,_) time = do
            let light' = Vec3 0 0 3
                ldir = Vec3 0 0 (-1)
                lup = Vec3 0 1 0
                light = light' + (Vec3 d 0 0)
                d = 5 * sin (0.3 * time)
                lm = fromProjective (lookat light (light + ldir) lup)
                cm = fromProjective (lookat cam (cam + dir) up)
                pm = perspective 0.1 50 (pi/2) (fromIntegral w / fromIntegral h)
                lpm = perspective 0.1 100 (pi/(1.3 + 0.2 * sin (2.7 * time))) (fromIntegral w / fromIntegral h)
            do
                timeSetter time
                scaleU $! 1 --512 / fromIntegral w
                scaleV $! 1 --512 / fromIntegral h
                matSetter $! mat4ToM44F $! cm .*. pm
                lightSetter $! mat4ToM44F $! lm .*. lpm
                --lightSetter2 $! mat4ToM44F $! lm .*. pm
            --putStrLn $ C.secs t ++ " - worldViewProj uniform setup via STM"
            setSize (fromIntegral w) (fromIntegral h)
            let s = sin t * 0.5 + 0.5
                t = 1.5 * time
            downU s
            upU (s+0.01)

            return ()
    r <- effectful3 setupGFX windowSize cam time
    return r

-- Continuous camera state (rotated with mouse, moved with arrows)
userCamera :: Real p => Vec3 -> Signal (Float, Float) -> Signal (Bool, Bool, Bool, Bool, Bool)
           -> SignalGen p (Signal (Vec3, Vec3, Vec3, (Float, Float)))
userCamera p mposs keyss = transfer2 (p,zero,zero,(0,0)) calcCam mposs keyss
  where
    d0 = Vec4 0 0 (-1) 1
    u0 = Vec4 0 1 0 1
    calcCam dt (dmx,dmy) (ka,kw,ks,kd,turbo) (p0,_,_,(mx,my)) = (p',d,u,(mx',my'))
      where
        f0 c n = if c then (&+ n) else id
        p'  = foldr1 (.) [f0 ka (v &* (-t)),f0 kw (d &* t),f0 ks (d &* (-t)),f0 kd (v &* t)] p0
        k   = if turbo then 5 else 1
        t   = k * realToFrac dt
        mx' = dmx + mx
        my' = dmy + my
        rm  = fromProjective $ rotationEuler $ Vec3 (mx' / 100) (my' / 100) 0
        d   = trim $ rm *. d0 :: Vec3
        u   = trim $ rm *. u0 :: Vec3
        v   = normalize $ d &^ u
