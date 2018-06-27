{-# LANGUAGE OverloadedStrings, PackageImports, TypeOperators, MultiParamTypeClasses, DataKinds #-}

import qualified Graphics.UI.GLFW as GLFW
import Control.Applicative hiding (Const)
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import Data.IORef
import Data.Word
import Data.Vect
import Data.Vect.Float.Instances ()
import FRP.Elerea.Param
import qualified Data.ByteString.Char8 as SB
import qualified Data.Trie as T
import qualified Data.Vector.Storable as SV
import System.Environment

import LambdaCube.GL

import Graphics.Rendering.OpenGL.Raw.Core32
import LambdaCube.GL.Mesh
import Codec.Image.STB hiding (Image)

import Data.Maybe
import Data.Bitmap.Pure

import Utility
import Utils
import ShaderToy
import BuiltinVec

n_time = "time"
n_size = "size"
n_background = "background"

smp n uv = texture' (Sampler LinearFilter ClampToEdge $ TextureSlot n $ Texture2D (Float RGBA) n1) uv
background tex = renderScreen $ \uv -> FragmentOut $ smp tex uv :. ZT

main :: IO ()
main = do
    let lcnet :: Exp Obj (Image 1 V4F)
        --lcnet = fxFakeRipple (Uni $ IFloat n_time) (Uni $ IV2F n_size) (background n_background)
        --lcnet = fxWarping (Uni $ IFloat n_time) (Uni $ IV2F n_size) (background n_background)
        lcnet = fxMotionBlur (Uni $ IFloat n_time) (Uni $ IV2F n_size) (background n_background)

    (win, windowSize) <- initWindow "LC DSL 2D Demo" 512 512

    renderer <- compileRenderer $ ScreenOut lcnet
    print $ slotUniform renderer
    print $ slotStream renderer
    print "renderer created"

    (mousePosition,mousePositionSink) <- external (0,0)
    (fblrPress,fblrPressSink) <- external (False,False,False,False,False)

    initUtility renderer

    args <- getArgs
    let slotU           = uniformSetter renderer
        setBackground   = uniformFTexture2D n_background slotU
        draw _          = render renderer >> GLFW.swapBuffers win >> GLFW.pollEvents
        bgname   = case args of
            []  -> "textures/rusty_metal.jpg"
            n:_ -> n
    Right img <- loadImage bgname
    setBackground =<< compileTexture2DRGBAF True False img

    s <- fpsState
    sc <- start $ do
        u <- scene (setScreenSize renderer) slotU windowSize mousePosition fblrPress
        return $ draw <$> u
    driveNetwork sc (readInput win s mousePositionSink fblrPressSink)

    dispose renderer
    print "renderer destroyed"
    GLFW.destroyWindow win
    GLFW.terminate

scene :: (Word -> Word -> IO ())
      -> T.Trie InputSetter
      -> Signal (Int, Int)
      -> Signal (Float, Float)
      -> Signal (Bool, Bool, Bool, Bool, Bool)
      -> SignalGen Float (Signal ())
scene setSize slotU windowSize mousePosition fblrPress = do
    time <- stateful 0 (+)
    let setTime = uniformFloat n_time slotU
        setFXSize = uniformV2F n_size slotU
        setupGFX (w,h) t' (x,y) = do
            setSize (fromIntegral w) (fromIntegral h)
            let t = 1.5 * t'
            setTime t
            setFXSize $ V2 (fromIntegral w) (fromIntegral h)
            return ()
    r <- effectful3 setupGFX windowSize time mousePosition
    return r
