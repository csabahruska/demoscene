{-# LANGUAGE OverloadedStrings, PackageImports, TypeOperators, DataKinds, FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

import "GLFW-b" Graphics.UI.GLFW as GLFW
import Control.Monad
import Data.Monoid
import Data.Vect
import qualified Data.ByteString.Char8 as BS
import qualified Data.Trie as T
import qualified Data.Vector.Storable as SV

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

texturing1D :: Wire -> Exp Obj (FrameBuffer 1 (Float,V4F)) -> Exp Obj (VertexStream Triangle (V2F)) -> Exp Obj (FrameBuffer 1 (Float,V4F))
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
            Wire1D _ f -> modelViewProj @*. (pack' $ V4 fx fy fz (Const 1))
              where
                (fx, fy, fz) = f x

        V2 x y = unpack' uv

    fragmentShader :: Exp F V2F -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    fragmentShader uv = FragmentOutRastDepth $ color tex uv :. ZT
      where
        tex = TextureSlot "myTextureSampler" $ Texture2D (Float RGBA) n1

texturing2D :: Wire -> Exp Obj (FrameBuffer 1 (Float,V4F)) -> Exp Obj (VertexStream Triangle (V2F)) -> Exp Obj (FrameBuffer 1 (Float,V4F))
texturing2D wire emptyFB objs = Accumulate fragmentCtx PassAll fragmentShader fragmentStream emptyFB
  where
    rasterCtx :: Wire -> RasterContext Triangle
    rasterCtx (Wire2D twosided _ _ _ _) = TriangleCtx (if twosided then CullNone else CullFront CW) (PolygonFill {-PolygonLine 1-}) NoOffset LastVertex

    fragmentCtx :: AccumulationContext (Depth Float :+: (Color (V4 Float) :+: ZZ))
    fragmentCtx = AccumulationContext Nothing $ DepthOp Less depthWrite :.ColorOp blending (one' :: V4B):.ZT

    (depthWrite, blending) = case wire of
        Wire2D _ _ _ _ Nothing -> (True, NoBlending)
        Wire2D _ _ _ _ (Just _) -> (True, blend)

--    fragmentStream :: Exp Obj (FragmentStream 1 V2F)
    fragmentStream = Rasterize (rasterCtx wire) primitiveStream

--    primitiveStream :: Exp Obj (PrimitiveStream Triangle () 1 V V2F)
    primitiveStream = Transform vertexShader objs

    modelViewProj :: Exp f M44F
    modelViewProj = Uni (IM44F "MVP")

    modelView :: Exp f M44F
    modelView = Uni (IM44F "MV")

    proj :: Exp f M44F
    proj = Uni (IM44F "P")

    prj1 a = drop4 $ modelView @*. snoc a 1
    prj0 a = drop4 $ modelView @*. snoc a 0

    vertexShader :: Exp V (V2F) -> VertexOut () (V2F, V3F, V4F, Float)
    vertexShader uv = VertexOut v4 (Const 1) ZT (Smooth uv :. Smooth ns :. Smooth pos :. Flat alpha :. ZT)
      where
--        v4 :: Exp V V4F
        (v4, ns, pos, alpha) = case wire of
            Wire2D _ _ _ f transp -> (modelViewProj @*. ps, ns_, modelView @*. ps, transparency)
              where
                ps = pack' $ V4 fx fy fz (Const 1)
                ((fx, fy, fz), normals) = f x y
                transparency = case transp of
                    Nothing -> Const 1
                    Just trp -> trp x y
                ns_ = case normals of
                    Nothing -> Const zero' --undefined
                    Just (nx, ny, nz) -> pack' $ V3 nx ny nz

        V2 x y = unpack' uv

    --fragmentShader :: Exp F (V2F,V3F) -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    fragmentShader (untup4 -> (uv, ns, pos', alpha)) = FragmentOutRastDepth $ c' :. ZT
      where
        tex = imgToTex textImg -- TextureSlot "myTextureSampler" $ Texture2D (Float RGBA) n1
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

main' :: [Wire] -> IO ()
main' wires = do
    initialize
    openWindow defaultDisplayOptions
        { displayOptions_width              = 1024
        , displayOptions_height             = 768
        , displayOptions_openGLVersion      = (3,2)
        , displayOptions_openGLProfile      = CoreProfile
        , displayOptions_numDepthBits       = 24
        }
    setWindowTitle "LambdaCube 3D Textured Cube"

    let emptyFB :: Exp Obj (FrameBuffer 1 (Float,V4F))
        emptyFB = FrameBuffer (DepthImage n1 1000:.ColorImage n1 (V4 0 0 0.4 1):.ZT)

        frameImage' = PrjFrameBuffer "" tix0 $ foldl addWire emptyFB $ zip (map (("stream" <>) . BS.pack . show) [0..]) wires

        frameImage :: Exp Obj (Image 1 V4F)
        frameImage = renderScreen $ (FragmentOut.(:.ZT).fxVignette vignette frameImage')
        --frameImage = renderScreen $ (FragmentOut.(:.ZT).fxScanlines sl frameImage')
        sl    = scanlines { scanlinesFrequency = floatF 128
                          , scanlinesHigh = Const $ V4 0.9 1 1 1
                          , scanlinesLow = Const $ V4 0.45 0.5 0.5 1
                          }

--        addWire :: -> (String, 
        addWire fb (name, wire@(Wire1D {})) = texturing1D wire fb (Fetch name Triangles (IV2F "position"))
        addWire fb (name, wire@(Wire2D {})) = texturing2D wire fb (Fetch name Triangles (IV2F "position"))

        frameImage'' = PrjFrameBuffer "" tix0 $ textRender $ texToFB $ imgToTex frameImage'

    renderer <- compileRenderer $ ScreenOut frameImage''

    initUtility renderer

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

    let uniformMap      = uniformSetter renderer
        texture         = uniformFTexture2D "myTextureSampler" uniformMap
        mvp             = uniformM44F "MVP" uniformMap
        mv              = uniformM44F "MV" uniformMap
        proj            = uniformM44F "P" uniformMap
        time            = uniformFloat "time" uniformMap
        setWindowSize   = setScreenSize renderer

    setWindowSize 1024 768
    Right img <- loadImage "textures/rusty_metal.jpg"
    texture =<< compileTexture2DRGBAF True False img

    forM_ (zip [0..] wires) $ \(n, w) -> do
        gpuCube <- compileMesh $ case w of
            Wire1D i _ -> line i
            Wire2D _ i j _ _ -> grid i j
        addMesh renderer ("stream" <> BS.pack (show n)) gpuCube []

    let cm  = fromProjective (lookat (Vec3 4 3 3) (Vec3 0 0 0) (Vec3 0 1 0))
        pm  = perspective 0.1 100 (pi/4) (1024 / 768)
        loop = do
            t <- getTime
            let angle = pi / 2 * realToFrac t * 0.3
                mm = fromProjective $ rotationEuler $ Vec3 angle 0 0
            mvp $! mat4ToM44F $! mm .*. cm .*. pm
            mv $! mat4ToM44F $! mm .*. cm
            proj $! mat4ToM44F $! pm
            time $ realToFrac t
            render renderer
            swapBuffers

            k <- keyIsPressed KeyEsc
            unless k $ loop

    initAudio 2 44100 1024
    smp <- sampleFromFile "music/Take_Them.ogg" 1
    soundPlay smp 1 1 0 1

    loop
    finishAudio

    dispose renderer
    closeWindow
