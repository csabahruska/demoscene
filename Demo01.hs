{-# LANGUAGE OverloadedStrings, PackageImports, TypeOperators, DataKinds #-}

import "GLFW-b" Graphics.UI.GLFW as GLFW
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import Data.Word
import Data.Vect
import Data.Vect.Float.Instances ()
import qualified Data.Trie as T
import Data.IORef

import LambdaCube.GL
import LambdaCube.GL.Mesh

import Codec.Image.STB hiding (Image)

import Graphics.Rendering.OpenGL.Raw.Core32
import qualified Data.Vector.Storable as SV

import Utility
import BuiltinVec

import System.Random

{-
  demo skeleton:
    done - 1.5 - text stripes using lafonten
    done - distorted glass effect
    done - 1.5 - loading screen

    camera support it should run on a cuve calculated on CPU

    switch between 2 scenes
    modulate an effect parameter with a time varying function
    shadow mapping
    preprocess music store various events list
-}

points :: Mesh
points = Mesh
    { mAttributes   = T.fromList
        [ ("position", A_V2F $ SV.replicate 1000 $ V2 0 0)
        , ("vid", A_Int $ SV.fromList [0..7])
        ]
    , mPrimitive    = P_Points
    , mGPUData      = Nothing
    }

sprites :: Exp Obj (Image 1 V4F)
sprites = PrjFrameBuffer "" tix0 $ Accumulate fragCtx PassAll frag rast clean
  where
    fragCtx = AccumulationContext Nothing $ ColorOp blend (one' :: V4B):.ZT
    blend   = Blend (FuncAdd, FuncAdd) ((SrcAlpha, One), (SrcAlpha, OneMinusSrcAlpha)) zero'
    clean   = FrameBuffer (ColorImage n1 (V4 0 0 0 1):.ZT)
    rast    = Rasterize rCtx prims
    rCtx    = PointCtx ProgramPointSize 10 UpperLeft
    prims   = Transform vert input
    input   = Fetch "points" Points (IV2F "position")

    vert :: Exp V V2F -> VertexOut () ()
    vert uv = VertexOut (vec4' uv (floatV 1) (floatV 1)) (Const 20) ZT ZT

    offset = Uni (IV2F "offset") :: Exp F V2F
    smp n uv = texture' (Sampler LinearFilter ClampToEdge $ TextureSlot n $ Texture2D (Float RGBA) n1) uv
    frag :: Exp F () -> FragmentOut (Color V4F :+: ZZ)
    frag _ = FragmentOut $ (smp "explosion" $ (pointCoord' @* floatF 0.25 @+ offset)) :. ZT

------

texturing :: Exp Obj (VertexStream Triangle (V3F,V2F)) -> Exp Obj (FrameBuffer 1 (Float,V4F))
texturing objs = Accumulate fragmentCtx PassAll fragmentShader fragmentStream emptyFB
  where
    rasterCtx :: RasterContext Triangle
    rasterCtx = TriangleCtx (CullFront CW) PolygonFill NoOffset LastVertex

    fragmentCtx :: AccumulationContext (Depth Float :+: (Color (V4 Float) :+: ZZ))
    fragmentCtx = AccumulationContext Nothing $ DepthOp Less True:.ColorOp NoBlending (one' :: V4B):.ZT

    emptyFB :: Exp Obj (FrameBuffer 1 (Float,V4F))
    emptyFB = FrameBuffer (DepthImage n1 1000:.ColorImage n1 (V4 0 0 0.4 1):.ZT)

    fragmentStream :: Exp Obj (FragmentStream 1 V2F)
    fragmentStream = Rasterize rasterCtx primitiveStream

    primitiveStream :: Exp Obj (PrimitiveStream Triangle () 1 V V2F)
    primitiveStream = Transform vertexShader objs

    modelViewProj :: Exp V M44F
    modelViewProj = Uni (IM44F "MVP")

    vertexShader :: Exp V (V3F,V2F) -> VertexOut () V2F
    vertexShader puv = VertexOut v4 (Const 1) ZT (Smooth uv:.ZT)
      where
        v4 :: Exp V V4F
        v4 = modelViewProj @*. v3v4 p
        (p,uv) = untup2 puv

    fragmentShader :: Exp F V2F -> FragmentOut (Depth Float :+: Color V4F :+: ZZ)
    fragmentShader uv = FragmentOutRastDepth $ color tex uv :. ZT
      where
        tex = TextureSlot "myTextureSampler" $ Texture2D (Float RGBA) n1

v3v4 :: Exp s V3F -> Exp s V4F
v3v4 v = let V3 x y z = unpack' v in pack' $ V4 x y z (Const 1)

color t uv = texture' (smp t) uv
smp t = Sampler LinearFilter ClampToEdge t

masked mask img = renderScreen frag
  where
    frag :: Exp F V2F -> FragmentOut (Color V4F :+: ZZ)
    frag uv = FragmentOut $ color :. ZT
      where
        color = smp img uv @* smp mask uv
        sizeI = 512 :: Word32
        smp i coord = texture' (Sampler LinearFilter ClampToEdge $ Texture (Texture2D (Float RGBA) n1) (V2 sizeI sizeI) NoMip [i]) coord

main :: IO ()
main = do
    windowSize <- initCommon "Exp14 Demo"
    renderer <- compileRenderer $ ScreenOut sprites

    let monkey :: Exp Obj (Image 1 V4F)
        monkey = PrjFrameBuffer "" tix0 $ texturing $ Fetch "stream" Triangles (IV3F "position", IV2F "UVTex")

        frameImage :: Exp Obj (Image 1 V4F)
        frameImage = masked sprites monkey

    renderer2 <- compileRenderer $ ScreenOut frameImage

    initUtility renderer
    initUtility renderer2

    mesh <- loadMesh "models/Monkey.lcmesh"
    obj <- addMesh renderer2 "stream" mesh []

    compiledPoints <- compileMesh points
    obj <- addMesh renderer "points" compiledPoints []
    obj <- addMesh renderer2 "points" compiledPoints []

    let objU    = objectUniformSetter obj
        slotU   = uniformSetter renderer
        uniformMap   = uniformSetter renderer2
        fname   = "textures/Explosion.png"
        texture         = uniformFTexture2D "myTextureSampler" uniformMap
        mvp             = uniformM44F "MVP" uniformMap

    Right img <- loadImage fname
    tex <- compileTexture2DRGBAF False True img
    uniformFTexture2D "explosion" slotU tex
    uniformFTexture2D "explosion" uniformMap tex
    texture tex

    Right maskImg <- loadImage "textures/particle_base.png"
    maskTex <- compileTexture2DRGBAF False True maskImg

    let loop = do
            (w,h) <- readIORef windowSize
            setScreenSize renderer (fromIntegral w) (fromIntegral h)
            setScreenSize renderer2 (fromIntegral w) (fromIntegral h)

            t <- getTime
            updateData t
            render renderer2
            swapBuffers

            k <- keyIsPressed KeyEsc
            unless k $ loop

        updateData t = do
          (w,h) <- readIORef windowSize
          --updateMesh :: Mesh -> [(ByteString,MeshAttribute)] -> Maybe MeshPrimitive -> IO ()
          v <- replicateM 1000 $ do
            x <- randomRIO (-1,1)
            y <- randomRIO (-1,1)
            return $ V2 x y
          updateMesh compiledPoints [("position",A_V2F $ SV.fromList v)] Nothing
          let angle = pi / 2 * realToFrac t
              mm = fromProjective $ rotationEuler $ Vec3 angle 0 0
          let cm  = fromProjective (lookat (Vec3 4 3 3) (Vec3 0 0 0) (Vec3 0 1 0))
              pm  = perspective 0.1 100 (pi/4) (fromIntegral w / fromIntegral h)
          mvp $! mat4ToM44F $! mm .*. cm .*. pm

    resetTime
    loop

    dispose renderer
    print "renderer destroyed"
    closeWindow

-- OpenGL/GLFW boilerplate

initCommon :: String -> IO (IORef (Int, Int))
initCommon title = do
    initialize
    openWindow defaultDisplayOptions
        { displayOptions_numRedBits         = 8
        , displayOptions_numGreenBits       = 8
        , displayOptions_numBlueBits        = 8
        , displayOptions_numAlphaBits       = 8
        , displayOptions_numDepthBits       = 24
        , displayOptions_width              = 1280
        , displayOptions_height             = 720
        , displayOptions_windowIsResizable  = True
        , displayOptions_openGLVersion      = (3,2)
        , displayOptions_openGLProfile      = CoreProfile
        , displayOptions_displayMode    = Fullscreen
        }
    setWindowTitle title

    windowSize <- newIORef (0,0)
    setWindowSizeCallback $ \w h -> do
        glViewport 0 0 (fromIntegral w) (fromIntegral h)
        putStrLn $ "window size changed " ++ show (w,h)
        writeIORef windowSize (fromIntegral w, fromIntegral h)

    return windowSize

