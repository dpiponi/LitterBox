-- Probably need to delete old program
-- Add time

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

import Control.Monad
import Data.Int
import Data.Time
import Foreign.Marshal.Array
import Foreign.Ptr
import Control.Concurrent
import Control.Concurrent.Chan
import Foreign.Storable
import qualified SDL
import Prelude hiding (init)
import Data.String
import Graphics.Rendering.OpenGL as GL
import SDL.Vect
import Control.Monad.State as S
import Control.Lens
import Control.Applicative

import System.FSNotify
import System.FilePath

import Control.Exception
import qualified Data.ByteString as B

data ShaderInfo = ShaderInfo GL.ShaderType FilePath
    deriving (Eq, Ord, Show)

loadShaders :: [ShaderInfo] -> IO (Maybe GL.Program)
loadShaders infos =
   createProgram `bracketOnError` deleteObjectName $ \program -> do
      ok <- loadCompileAttach program infos
      if ok
          then do
              linkAndCheck program
              return (Just program)
          else do
              return Nothing

linkAndCheck :: GL.Program -> IO Bool
linkAndCheck = checked linkProgram linkStatus programInfoLog "link"

loadCompileAttach :: GL.Program -> [ShaderInfo] -> IO Bool
loadCompileAttach _ [] = return True
loadCompileAttach program (ShaderInfo shType source : infos) =
   createShader shType `bracketOnError` deleteObjectName $ \shader -> do
      src <- catch (B.readFile source) (\(e :: IOException) -> return "")
      let src' = if shType == FragmentShader then "#version 110\nuniform vec2 iResolution; uniform vec2 iMouse; uniform float iTime;\n#line 0\n" `B.append` src `B.append` "\nvoid main() { vec4 color; mainImage(color, gl_FragCoord.xy); gl_FragColor = color; }" else src
      shaderSourceBS shader $= src'
      ok <- compileAndCheck shader
      if ok
          then do
              attachShader program shader
              loadCompileAttach program infos
          else return False

compileAndCheck :: GL.Shader -> IO Bool
compileAndCheck = checked compileShader compileStatus shaderInfoLog "compile"

checked :: (t -> IO ())
        -> (t -> GL.GettableStateVar Bool)
        -> (t -> GL.GettableStateVar String)
        -> String
        -> t
        -> IO Bool
checked action getStatus getInfoLog message object = do
    action object
    ok <- GL.get (getStatus object)
    unless ok $ do
        infoLog <- GL.get (getInfoLog object)
        putStrLn (message ++ " log: " ++ infoLog)
        putStrLn $ "OK=" ++ show ok
    return ok

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

data World = World {
        _windowSize :: (Int32, Int32),
        _shaderProgram :: Maybe GL.Program,
        _mainWindow :: SDL.Window,
        _fsEvents :: MVar Event, -- not a queue. Change.
        _startTime :: UTCTime
    }

$(makeLenses 'World)

installShaders :: IO (Maybe GL.Program)
installShaders = do
    loadShaders [
        ShaderInfo VertexShader "litterbox.vert",
        ShaderInfo FragmentShader "litterbox.frag"] >>= \case

        Nothing -> return Nothing
        Just program -> do
            currentProgram $= Just program
            attribLocation program "vPosition" $= AttribLocation 0
            setShaderWindow program (512, 512)
            return (Just program)

init :: MVar Event -> IO World
init events = do
    window <- initWindow

    SDL.showWindow window
    _ <- SDL.glCreateContext window
    SDL.swapInterval $= SDL.SynchronizedUpdates

    startTime <- getCurrentTime
    let world = World (512, 512) Nothing window events startTime
    program <- installShaders
    return $ world & shaderProgram %~ (<|>) program

vertices :: [Vertex2 GLfloat]
vertices = [
    Vertex2 (-1.00) (-1.00),  -- Triangle 1
    Vertex2   1.00  (-1.00),
    Vertex2 (-1.00)   1.00,
    Vertex2   1.00  (-1.00),  -- Triangle 2
    Vertex2   1.00    1.00,
    Vertex2 (-1.00)   1.00]

render :: SDL.Window -> IO ()
render window = do
    clearColor $= Color4 0.2 0.0 0.1 1
    clear [ColorBuffer]
    vertexAttribArray (AttribLocation 0) $= Enabled
    withArray vertices $ \ptr ->
        vertexAttribPointer (AttribLocation 0) $=
          (ToFloat, VertexArrayDescriptor 2 Float 0 ptr)
    drawArrays Triangles 0 6
    vertexAttribArray (AttribLocation 0) $= Disabled
    flush
    SDL.glSwapWindow window

setShaderTime :: GL.Program -> Float -> IO ()
setShaderTime program time = do
    loc <- uniformLocation program "iTime"
    uniform loc $= time

setShaderWindow :: GL.Program -> (Int32, Int32) -> IO ()
setShaderWindow program (w, h) = do
    loc <- uniformLocation program "iResolution"
    uniform loc $= Vector2 (fromIntegral w) (fromIntegral h::Float)

setShaderMouse :: GL.Program -> (Int32, Int32) -> IO ()
setShaderMouse program (w, h) = do
    loc <- uniformLocation program "iMouse"
    uniform loc $= Vector2 (fromIntegral w) (fromIntegral h::Float)

watching :: String -> Bool
watching path = 
    let name = takeFileName path
    in name == "litterbox.frag" || name == "litterbox.vert"

doIfUpdate :: StateT World IO () -> StateT World IO ()
doIfUpdate cmd = do
    events <- use fsEvents
    liftIO (tryTakeMVar events) >>= \case
        Nothing -> return ()
        Just (Added path time) -> when (watching path) cmd
        Just (Removed path time) -> when (watching path) cmd
        Just (Modified path time) -> when (watching path) cmd

withProgram :: (GL.Program -> StateT World IO ()) -> StateT World IO ()
withProgram cmd = 
    use shaderProgram >>= \case
        Nothing -> return ()
        Just program -> cmd program

updateShaderIfNeeded :: StateT World IO ()
updateShaderIfNeeded = doIfUpdate $ do
    program <- liftIO installShaders
    shaderProgram %= (<|>) program

reshape :: (Int32, Int32) -> StateT World IO ()
reshape (w, h) = do
    withProgram $ \program -> liftIO $ setShaderWindow program (w, h)
    liftIO $ viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
    windowSize .= (w, h)

mouse :: (Int32, Int32) -> StateT World IO ()
mouse (x, y) = withProgram $ \program -> do
        (_, h) <- use windowSize
        liftIO $ setShaderMouse program (x, h-y)

timeSinceStart:: StateT World IO Float
timeSinceStart = realToFrac <$> (diffUTCTime <$> liftIO getCurrentTime <*> use startTime)

handleUIEvent :: SDL.Event -> StateT World IO ()
handleUIEvent = \case 
    SDL.Event time (SDL.WindowResizedEvent (SDL.WindowResizedEventData win (V2 w h))) ->
        reshape (w, h)
    SDL.Event time (SDL.MouseMotionEvent (SDL.MouseMotionEventData { SDL.mouseMotionEventPos = xy })) ->
        let P (V2 x y) = xy in mouse (x, y)
    _ -> return ()

loop :: StateT World IO ()
loop = do
    window <- use mainWindow
    interval <- timeSinceStart
    withProgram $ \program -> liftIO $ setShaderTime program interval
    liftIO $ render window
    updateShaderIfNeeded
    events <- liftIO SDL.pollEvents
    mapM_ handleUIEvent events
    loop

initWindow :: IO SDL.Window
initWindow = SDL.createWindow "LitterBox"
                SDL.defaultWindow {
                    SDL.windowInitialSize = V2 (fromIntegral 512) (fromIntegral 512),
                    SDL.windowResizable = True,
                    SDL.windowOpenGL = Just $ SDL.OpenGLConfig {
                        SDL.glColorPrecision = V4 8 8 8 0,
                        SDL.glDepthPrecision = 24,
                        SDL.glStencilPrecision = 8,
                        SDL.glMultisampleSamples = 1,
                        SDL.glProfile = SDL.Compatibility SDL.Normal 2 1
                        }}

main :: IO ()
main = do
    SDL.initialize [SDL.InitVideo]

    events <- newEmptyMVar -- XXX Not good
    world <- init events

    forkIO $ withManager $ \mgr -> do
        watchDir mgr "." (const True) (putMVar events)
        forever $ threadDelay 1000

    evalStateT loop world
