{-# LANGUAGE OverloadedStrings, PackageImports, TypeOperators, DataKinds, FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

import System.Environment
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Control.Monad
import Control.Arrow
import Control.Applicative hiding (Const)
import Data.Monoid
import Data.Vect
import Data.Function
import qualified Data.Traversable as Trav
import qualified Data.ByteString.Char8 as BS
import qualified Data.Trie as T
import qualified Data.Vector.Storable as SV
import Control.Concurrent
import Data.Time.Clock
import Data.IORef

import Sound.ProteaAudio

import Geometry
import Utility
import Blur
import Scanlines
import Vignette

import LambdaCube.GL
import LambdaCube.GL.Mesh

import LambdaCube.Font.Atlas
import LambdaCube.Font.Common hiding (floatF,floatV,v3v4)
import qualified LambdaCube.Font.SimpleDistanceField as SDF
import qualified LambdaCube.Font.CompositeDistanceField as CDF

import Graphics.Text.TrueType

import Codec.Image.STB hiding (Image)

import KnotsLC
import qualified Knot

import Math.Noise hiding (zero)
import Math.Noise.Modules.Billow
import Data.Maybe
import Data.Bitmap.Pure

renderTextureSize = 1024

distortFX :: Exp Obj (Image 1 V4F) -> Exp Obj (FrameBuffer 1 V4F)
distortFX img = Accumulate fragCtx PassAll frag rast clear
  where
    fragCtx = AccumulationContext Nothing $ ColorOp NoBlending (one' :: V4B):.ZT
    clear   = FrameBuffer (ColorImage n1 (V4 1 0 0 1):.ZT)
    rast    = Rasterize triangleCtx prims
    prims   = Transform vert input
    input   = Fetch "ScreenQuad" Triangles (IV2F "position")

    vert :: Exp V V2F -> VertexOut () V2F
    vert uv = VertexOut v4 (Const 1) ZT (NoPerspective uv:.ZT)
      where
        v4      = pack' $ V4 u v (floatV 1) (floatV 1)
        V2 u v  = unpack' uv

    up = Uni (IFloat "up")
    down = Uni (IFloat "down")
    time = Uni (IFloat "time")
    on = Uni (IBool "on")
    frag :: Exp F V2F -> FragmentOut (Color V4F :+: ZZ)
    frag uv' = FragmentOut $ Cond on c texel2 :. ZT
      where
        c = Cond (down @< r @&& r @< up) (mask @+ (texel2 @* floatF 0.3)) $
            Cond (down @< r) (pack' $ V4 tR tG tB (floatF 1)) texel2
        texel = smp "DistorsionTex" uv
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
        smp' i uv = texture' (Sampler LinearFilter ClampToEdge $ Texture (Texture2D (Float RGBA) n1) (V2 renderTextureSize renderTextureSize) NoMip [i]) uv


useCompositeDistanceField = True

textImg = PrjFrameBuffer "" tix0 $ textRender $ FrameBuffer (ColorImage n1 (V4 0 0 0 1) :. ZT)

textRender :: Exp Obj (FrameBuffer 1 V4F) -> Exp Obj (FrameBuffer 1 V4F)
textRender = renderText
  where
    renderText = Accumulate textFragmentCtx PassAll textFragmentShader textFragmentStream
    --emptyBuffer = 
    rasterCtx = TriangleCtx CullNone PolygonFill NoOffset LastVertex

    textFragmentCtx = AccumulationContext Nothing (ColorOp textBlending (V4 True True True True) :. ZT)
    textBlending = Blend (FuncAdd, FuncAdd) ((One, One), (OneMinusSrcAlpha, One)) zero'
    textFragmentStream = Rasterize rasterCtx textStream
    textStream = Transform vertexShader (Fetch "textMesh" Triangles (IV2F "position", IV2F "uv"))

    vertexShader attr = VertexOut point (floatV 1) ZT (Smooth uv :. ZT)
      where
        point = v3v4 (transform @*. v2v3 pos)
        transform = Uni (IM33F "textTransform") :: Exp V M33F
        (pos, uv) = untup2 attr

    textFragmentShader uv = FragmentOut (pack' (V4 result result result result) :. ZT)
      where
        result = step distance
        distance = case useCompositeDistanceField of
            False -> SDF.sampleDistance "fontAtlas" uv
            True -> CDF.sampleDistance "fontAtlas" uv
        step = smoothstep' (floatF 0.5 @- outlineWidth) (floatF 0.5 @+ outlineWidth)
        outlineWidth = Uni (IFloat "outlineWidth") :: Exp F Float

texturing1D :: Wire Int (Exp V Float) -> Exp Obj (FrameBuffer 1 (Float,V4F)) -> Exp Obj (VertexStream Triangle (V2F)) -> Exp Obj (FrameBuffer 1 (Float,V4F))
texturing1D wire emptyFB objs = Accumulate fragmentCtx PassAll fragmentShader fragmentStream emptyFB
  where
    rasterCtx :: RasterContext Triangle
    rasterCtx = TriangleCtx CullNone{-(CullFront CW)-} (PolygonLine 1) NoOffset LastVertex

    fragmentCtx :: AccumulationContext (Depth Float :+: (Color (V4 Float) :+: ZZ))
    fragmentCtx = AccumulationContext Nothing $ DepthOp Less True:.ColorOp NoBlending (one' :: V4B):.ZT

    fragmentStream :: Exp Obj (FragmentStream 1 V2F)
    fragmentStream = Rasterize rasterCtx primitiveStream

    primitiveStream :: Exp Obj (PrimitiveStream Triangle () 1 V V2F)
    primitiveStream = Transform vertexShader objs

    modelViewProj :: Exp V M44F
    modelViewProj = Uni (IM44F "MVP")

    vertexShader :: Exp V (V2F) -> VertexOut () V2F
    vertexShader uv = VertexOut v4 (Const 1) ZT (Smooth uv:.ZT)
      where
        v4 :: Exp V V4F
        v4 = case wire of
            Wire1D {..} -> modelViewProj @*. (pack' $ V4 fx fy fz (Const 1))
              where
                Knot.V3 fx fy fz = wVertex1 x

        V2 x y = unpack' uv

    fragmentShader :: Exp F V2F -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    fragmentShader uv = FragmentOutRastDepth $ color tex uv :. ZT
      where
        tex = TextureSlot "myTextureSampler" $ Texture2D (Float RGBA) n1

data VertFrag where
    VertFrag :: forall a . GPU a
            => (Exp V (V2F) -> VertexOut () a)
            -> (Exp F a -> FragmentOut (Depth Float :+: Color V4F :+: ZZ))
            -> VertFrag

texturing2D :: Wire Int (Exp V Float) -> Exp Obj (FrameBuffer 1 (Float,V4F)) -> Exp Obj (VertexStream Triangle (V2F)) -> Exp Obj (FrameBuffer 1 (Float,V4F))
texturing2D wire@(Wire2D {..})
    = texturing2D_ wire vertFrag
  where
    vertFrag = case wire of
        Wire2D { wColor = Just fcolor } ->  VertFrag vertexShader fragmentShader
          where
            vertexShader :: Exp V (V2F) -> VertexOut () (V2F, V3F, V4F, V3F, Float)
            vertexShader uv = VertexOut v4 (Const 1) ZT (Smooth uv :. Smooth ns_ :. Smooth pos :. Smooth color :. Flat alpha :. ZT)
              where
                v4 = modelViewProj @*. ps
                pos = modelView @*. ps
                ps = pack' $ V4 fx fy fz (Const 1)
                Knot.V3 fx fy fz = wVertex $ Knot.V2 x y
                alpha = maybe (Const 1) (\trp -> trp $ Knot.V2 x y) wAlpha
                color = case fcolor $ Knot.V2 x y of Knot.V3 r g b -> pack' $ V3 r g b
                ns_ = case wNormal of
                    Nothing -> Const zero' --undefined
                    Just fn -> pack' $ V3 nx ny nz
                      where
                        Knot.V3 nx ny nz = fn $ Knot.V2 x y

                V2 x y = unpack' uv

--            fragmentShader :: Exp F (V2F,V3F,V4F,Float) -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
            fragmentShader (untup5 -> (uv, ns, pos', col, alpha)) = FragmentOutRastDepth $ (if wSimpleColor then clr else clr @* dot' n dlight) :. ZT
              where
                tex = {-imgToTex textImg -} TextureSlot "myTextureSampler" $ Texture2D (Float RGBA) n1
                clr = case unpack' col of (V3 r g b) -> pack' $ V4 r g b alpha -- color tex uv
                
                pos = v4v3 pos'
                pointlight = v3FF $ V3 1 1 1
                pointlight1 = v3FF $ V3 (-5) (-1) 1
                dlight = normalize' $ prj1 pointlight @- pos
                dlight1 = normalize' $ prj1 pointlight1 @- pos
                color0 = v4FF $ V4 0 0 1 1
                color1 = v4FF $ V4 1 0 0 1
                n = normalize' ns
                c = clr @* ((color0 @* dot' n dlight) @+ (color1 @* dot' n dlight1))
                c' = case unpack' c of (V4 r g b _) -> pack' $ V4 r g b alpha

        _ ->  VertFrag vertexShader fragmentShader
          where
            vertexShader :: Exp V (V2F) -> VertexOut () (V2F, V3F, V4F, Float)
            vertexShader uv = VertexOut v4 (Const 1) ZT (Smooth uv :. Smooth ns :. Smooth pos :. Flat alpha :. ZT)
              where
                (v4, ns, pos, alpha) = case wire of
                    Wire2D {..} -> (modelViewProj @*. ps, ns_, modelView @*. ps, transparency)
                      where
                        ps = pack' $ V4 fx fy fz (Const 1)
                        Knot.V3 fx fy fz = wVertex $ Knot.V2 x y
                        transparency = maybe (Const 1) (\trp -> trp $ Knot.V2 x y) wAlpha
                        ns_ = case wNormal of
                            Nothing -> Const zero' --undefined
                            Just fn -> pack' $ V3 nx ny nz
                              where
                                Knot.V3 nx ny nz = fn $ Knot.V2 x y

                V2 x y = unpack' uv

            fragmentShader :: Exp F (V2F,V3F,V4F,Float) -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
            fragmentShader (untup4 -> (uv, ns, pos', alpha)) = FragmentOutRastDepth $ c' :. ZT
              where
                tex = {-imgToTex textImg -} TextureSlot "myTextureSampler" $ Texture2D (Float RGBA) n1
                clr = color tex uv
                
                pos = v4v3 pos'
                pointlight = v3FF $ V3 1 1 1
                pointlight1 = v3FF $ V3 (-5) (-1) 1
                dlight = normalize' $ prj1 pointlight @- pos
                dlight1 = normalize' $ prj1 pointlight1 @- pos
                color0 = v4FF $ V4 0 0 1 1
                color1 = v4FF $ V4 1 0 0 1
                n = normalize' ns
                c = clr @* ((color0 @* dot' n dlight) @+ (color1 @* dot' n dlight1))
                c' = case unpack' c of (V4 r g b _) -> pack' $ V4 r g b alpha

    modelViewProj :: Exp f M44F
    modelViewProj = Uni (IM44F "MVP")

    modelView :: Exp f M44F
    modelView = Uni (IM44F "MV")

    prj1 a = drop4 $ modelView @*. snoc a 1
    prj0 a = drop4 $ modelView @*. snoc a 0



texturing2D_ :: Wire Int (Exp V Float) -> VertFrag -> Exp Obj (FrameBuffer 1 (Float,V4F)) -> Exp Obj (VertexStream Triangle (V2F)) -> Exp Obj (FrameBuffer 1 (Float,V4F))
texturing2D_ wire (VertFrag vertexShader fragmentShader) emptyFB objs = Accumulate fragmentCtx PassAll fragmentShader fragmentStream emptyFB
  where
    rasterCtx :: RasterContext Triangle
    rasterCtx = TriangleCtx (if wTwosided wire then CullNone else CullFront CW) (PolygonFill {-PolygonLine 1-}) NoOffset LastVertex

    fragmentCtx :: AccumulationContext (Depth Float :+: (Color (V4 Float) :+: ZZ))
    fragmentCtx = AccumulationContext Nothing $ DepthOp Less depthWrite :.ColorOp blending (one' :: V4B):.ZT

    (depthWrite, blending) = case wire of
        Wire2D { wAlpha = Nothing } -> (True, NoBlending)
        _ -> (False, blend)

--    fragmentStream :: Exp Obj (FragmentStream 1 V2F)
    fragmentStream = Rasterize rasterCtx primitiveStream

--    primitiveStream :: Exp Obj (PrimitiveStream Triangle () 1 V V2F)
    primitiveStream = Transform vertexShader objs

    proj :: Exp f M44F
    proj = Uni (IM44F "P")



v2v4 :: Exp s V2F -> Exp s V4F
v2v4 v = let V2 x y = unpack' v in pack' $ V4 x y (Const 0) (Const 1)

v3v4 :: Exp s V3F -> Exp s V4F
v3v4 v = let V3 x y z = unpack' v in pack' $ V4 x y z (Const 1)

v4v3 :: Exp s V4F -> Exp s V3F
v4v3 v = let V4 x y z _ = unpack' v in pack' $ V3 x y z

color t uv = texture' (smp t) uv
smp t = Sampler LinearFilter ClampToEdge t

copyImg img = renderScreen frag
  where
    frag :: Exp F V2F -> FragmentOut (Color V4F :+: ZZ)
    frag uv = FragmentOut $ color :. ZT
      where
        color = smp img uv
        sizeI = 1024 :: Word32
        smp i coord = texture' (Sampler LinearFilter ClampToEdge $ Texture (Texture2D (Float RGBA) n1) (V2 sizeI sizeI) NoMip [i]) coord

texToImg n = PrjFrameBuffer "" tix0 $ texToFB n

texToFB tex = renderScreen' frag
  where
    frag :: Exp F V2F -> FragmentOut (Color V4F :+: ZZ)
    frag uv = FragmentOut $ color tex uv :. ZT

slotTexToFB n = renderScreen' frag
  where
    frag :: Exp F V2F -> FragmentOut (Color V4F :+: ZZ)
    frag uv = FragmentOut $ color tex uv :. ZT
      where
        tex = TextureSlot n $ Texture2D (Float RGBA) n1

imgToTex img = Texture (Texture2D (Float RGBA) n1) (V2 sizeI sizeI) NoMip [img]
 where
  sizeI = 1024 :: Word32

main :: IO ()
main = main' =<< wires

textStyle = defaultTextStyle { textLetterSpacing = 0.0, textLineHeight = 1.25 }
fontOptions = defaultOptions { atlasSize = 1024, atlasLetterPadding = 2 }

streamName :: Int -> BS.ByteString
streamName = ("stream" <>) . BS.pack . show

type Time = Double

data Event
    = TurnOn Object
    | TurnOff Object
    | RecurrentEvent (Maybe Time){-end-} (Time -> IO ())

main' :: Wire Int (Exp V Float) -> IO ()
main' wires = do
    initialize
    openWindow defaultDisplayOptions
        { displayOptions_width              = 1024
        , displayOptions_height             = 768
        , displayOptions_openGLVersion      = (3,2)
        , displayOptions_openGLProfile      = CoreProfile
        , displayOptions_numDepthBits       = 24
        }
    setWindowTitle "Demo by Lambda"

    let emptyFB :: Exp Obj (FrameBuffer 1 (Float,V4F))
        emptyFB = FrameBuffer (DepthImage n1 1000:.ColorImage n1 (V4 0 0 0.4 1):.ZT)

        frameImage' = case wires of
            WHorizontal {..} -> PrjFrameBuffer "" tix0 $ foldl addWire emptyFB wWires

        frameImage :: Exp Obj (Image 1 V4F)
        frameImage = renderScreen $ (FragmentOut.(:.ZT).fxVignette vignette frameImage')
        --frameImage = renderScreen $ (FragmentOut.(:.ZT).fxScanlines sl frameImage')
        sl    = scanlines { scanlinesFrequency = floatF 128
                          , scanlinesHigh = Const $ V4 0.9 1 1 1
                          , scanlinesLow = Const $ V4 0.45 0.5 0.5 1
                          }

        addWire fb (WFadeOut{}) = fb
        addWire fb wire@(Wire1D {..}) = texturing1D wire fb (Fetch (streamName wInfo) Triangles (IV2F "position"))
        addWire fb wire@(Wire2D {..}) = texturing2D wire fb (Fetch (streamName wInfo) Triangles (IV2F "position"))
        addWire fb w = foldl addWire fb $ wWires w

        frameImage'' = PrjFrameBuffer "" tix0 $ textRender $ texToFB $ imgToTex $ PrjFrameBuffer "" tix0 $ distortFX frameImage'

        loadingImage = PrjFrameBuffer "" tix0 $ textRender $ FrameBuffer (ColorImage n1 (V4 0 0 0.4 1):.ZT)

    -- loading screen
    loadingRenderer <- compileRenderer $ ScreenOut loadingImage
    initUtility loadingRenderer
    setScreenSize loadingRenderer 1024 768

    do
      -- text
      Right font <- loadFontFile "fonts/Orbitron-Bold.ttf"
      let fontRenderer = if useCompositeDistanceField then CDF.fontRenderer else SDF.fontRenderer
          letterScale = 72
      atlas <- createFontAtlas font fontRenderer fontOptions { atlasLetterScale = letterScale }
      textMesh <- buildTextMesh atlas textStyle "Loading..."
      textBuffer <- compileMesh textMesh
      textObject <- addMesh loadingRenderer "textMesh" textBuffer []

      let uniforms = uniformSetter loadingRenderer
          letterScale = atlasLetterScale (atlasOptions atlas)
          letterPadding = atlasLetterPadding (atlasOptions atlas)
          scale = 0.1
          ofsX = -0.85
          ofsY = 0
      uniformFTexture2D "fontAtlas" uniforms (getTextureData atlas)

      uniformM33F "textTransform" uniforms (V3 (V3 (scale * 0.75) 0 0) (V3 0 scale 0) (V3 ofsX ofsY 1))
      uniformFloat "outlineWidth" uniforms (min 0.5 (fromIntegral letterScale / (768 * fromIntegral letterPadding * scale * sqrt 2 * 0.75)))

      render loadingRenderer
      swapBuffers

    -- main scene
    args <- getArgs
    let fname = case args of
          [] -> "textures/rusty_metal.jpg"
          (n:_) -> n
    Right imgPattern <- loadImage fname

    renderer <- compileRenderer $ ScreenOut frameImage''
    initUtility renderer

    -- text
    Right font <- loadFontFile "fonts/Zebulon.ttf"
    let fontRenderer = if useCompositeDistanceField then CDF.fontRenderer else SDF.fontRenderer
        letterScale = 72
    atlas <- createFontAtlas font fontRenderer fontOptions { atlasLetterScale = letterScale }
    textMesh <- buildTextMesh atlas textStyle "Hello, gorgeous world!"

    textBuffer <- compileMesh textMesh
    textObject <- addMesh renderer "textMesh" textBuffer []

    let uniforms = uniformSetter renderer
        letterScale = atlasLetterScale (atlasOptions atlas)
        letterPadding = atlasLetterPadding (atlasOptions atlas)
        scale = 0.1
        ofsX = -0.9
        ofsY = 0
    uniformFTexture2D "fontAtlas" uniforms (getTextureData atlas)

    uniformM33F "textTransform" uniforms (V3 (V3 (scale * 0.75) 0 0) (V3 0 scale 0) (V3 ofsX ofsY 1))
    uniformFloat "outlineWidth" uniforms (min 0.5 (fromIntegral letterScale / (768 * fromIntegral letterPadding * scale * sqrt 2 * 0.75)))

    -- distorsion
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

    uniformFTexture2D "DistorsionTex" uniforms =<< compileTexture2DRGBAF False True img

    let uniformMap      = uniformSetter renderer
        texture         = uniformFTexture2D "myTextureSampler" uniformMap
        mvp             = uniformM44F "MVP" uniformMap
        mv              = uniformM44F "MV" uniformMap
        proj            = uniformM44F "P" uniformMap
        time            = uniformFloat "time" uniformMap
        setWindowSize   = setScreenSize renderer

    setWindowSize 1024 768
    texture =<< compileTexture2DRGBAF True False imgPattern

    let addStreams :: Maybe Time -> Wire Int (Exp V Float) -> IO (Maybe Time, [(Time, Event)])
        addStreams Nothing _ = return (Nothing, [])
        addStreams t@(Just ti) c = case c of
            WHorizontal{..} -> (foldr (liftA2 max) t *** foldr merge []) . unzip <$> mapM (addStreams t) wWires
            WVertical{..} -> (id *** foldr merge []) <$> mapAccumLM addStreams t wWires
            WFadeOut{wDuration = Just len} -> return (t', [(ti, RecurrentEvent t' action)])
              where
                len' = realToFrac len
                t' = liftA2 (+) (Just len') t
                action time = print v >> volume v v
                  where v = realToFrac $ min 1 $ max 0 $ 1 - ((time - ti) / len')
            w -> do
                gpuCube <- compileMesh $ case w of
                    Wire1D {..} -> line wXResolution
                    Wire2D { wXResolution = i, wYResolution = j } -> grid i j
                obj <- addMesh renderer (streamName $ wInfo w) gpuCube []

                when (maybe True (> 0) t) $
                    enableObject obj False
                case wDuration w of
                    Nothing -> return (Nothing, [(ti, TurnOn obj)])
                    Just dur -> return (Just ti', [(ti, TurnOn obj), (ti', TurnOff obj)])
                        where ti' = ti + realToFrac dur

        merge = mergeBy (compare `on` fst)
        compare' (Just a) (Just b) = compare a b
        compare' Nothing Nothing = EQ
        compare' Nothing _ = GT
        compare' _ _ = LT

    (_end, schedule) <- addStreams (Just 0) wires

    timeRef <- newIORef =<< getCurrentTime

    let
        camCurve :: Knot.Curve
        --camCurve = Knot.mulSV3 2 . Knot.unKnot :: Knot.Curve
        camCurve = Knot.magnify 1 . Knot.lissajousKnot (Knot.V3 3 5 7) (Knot.V3 0.7 0.1 0)
        cm  = fromProjective (lookat (Vec3 4 3 3) (Vec3 0 0 0) (Vec3 0 1 0))
        pm  = perspective 0.1 100 (pi/4) (1024 / 768)
        loop [] = return ()
--        loop ((Nothing,_):_) = return ()
        loop schedule = do
            curTime <- getCurrentTime
            satrtTime <- readIORef timeRef
            let t' = realToFrac $ curTime `diffUTCTime` satrtTime
            let t = t'
            let (old, schedule_) = span (\(x, _) -> compare x t' == LT) schedule
            newEvents <- forM old $ \(i, event) -> do
                case event of
                    TurnOn obj -> enableObject obj True >> return []
                    TurnOff obj -> enableObject obj False >> return []
                    RecurrentEvent end action ->
                        if (compare' (Just t') end == LT)
                        then action t' >> return [(t', RecurrentEvent end action)]
                        else return []
            let schedule' = foldr merge [] newEvents `merge` schedule_
            let angle = pi / 2 * realToFrac t * 0.3
                mm = fromProjective $ rotationEuler $ Vec3 angle 0 0
                cam = cameraToMat4 $ curveToCameraPath camCurve (0.05 * realToFrac t)
            mvp $! mat4ToM44F $! cam .*. pm
            mv $! mat4ToM44F $! cam
            proj $! mat4ToM44F $! pm
            time $ realToFrac t
            let s = sin t' * 0.5 + 0.5
                t' = realToFrac $ 0.5 * t
                ti = floor t
            uniformBool "on" uniformMap $ ti `mod` 5 == 0
            uniformFloat "down" uniformMap $ s
            uniformFloat "up" uniformMap $ (s+0.01)
            render renderer
            swapBuffers

            k <- keyIsPressed KeyEsc
            unless k $ loop schedule'

    let audioOn = True
    smp <- if audioOn
      then do
        initAudio 2 44100 1024
        smp <- sampleFromFile "music/Take_Them.ogg" 1
        soundPlay smp 1 1 0 1
        return $ Just smp
      else return Nothing

--    resetTime

    writeIORef timeRef =<< getCurrentTime

    loop schedule

    case smp of
      Nothing -> return ()
      Just smp -> do
        soundStop smp
        soundStopAll

    dispose renderer
    closeWindow
--    soundStopAll

--    let waitAudio = do
--          n <- soundActive
--          if n == 0 then return () else waitAudio
--    waitAudio
--    finishAudio

mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy f [] xs = xs
mergeBy f xs [] = xs
mergeBy f (x:xs) (y:ys) = case f x y of
    LT -> x: mergeBy f xs (y:ys)
    _  -> y: mergeBy f (x:xs) ys

mapAccumLM :: Monad m => (st -> a -> m (st, b)) -> st -> [a] -> m (st, [b])
mapAccumLM f x [] = return (x, [])
mapAccumLM f x (y:ys) = do
    (x', y') <- f x y
    (x'', ys') <- mapAccumLM f x' ys
    return (x'', y':ys')


