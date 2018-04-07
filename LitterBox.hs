-- Probably need to delete old program

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

import Control.Monad
import Data.Int
import Data.Time
import Control.Concurrent
import System.Environment
import Control.Monad.STM
import qualified SDL
import Prelude hiding (init)
import qualified Graphics.Rendering.OpenGL as GL
import SDL.Vect
import Control.Monad.Except
import Control.Monad.State as S
import Control.Lens
import Control.Concurrent.STM.TChan

import System.FSNotify
import System.FilePath

import GLCode
import SDLCode

data World = World {
        _windowSize :: (Int32, Int32),
        _shaderProgram :: Maybe GL.Program,
        _mainWindow :: SDL.Window,
        _fsEvents :: TChan Event, -- not a queue. Change.
        _startTime :: UTCTime
    }

$(makeLenses ''World)

data Options = Options {
        _shaderDirectory :: FilePath
    } deriving Show

$(makeLenses ''Options)

-- Some duplication here XXX
init :: FilePath -> TChan Event -> IO World
init path events = do
    window <- initWindow

    start <- getCurrentTime
    let world = World (512, 512) Nothing window events start
    runExceptT (installShaders path) >>= \case
        Left e -> putStrLn ("Error: " ++ e) >> return world
        Right program -> return $ world & shaderProgram .~ Just program

watching :: String -> Bool
watching path = 
    let name = takeFileName path
    in name == "litterbox.frag" || name == "litterbox.vert"

doIfUpdate :: StateT World IO () -> StateT World IO ()
doIfUpdate cmd = do
    events <- use fsEvents
    io (atomically $ tryReadTChan events) >>= \case
        Nothing -> return ()
        Just (Added path _) -> when (watching path) cmd
        Just (Removed path _) -> when (watching path) cmd
        Just (Modified path _) -> when (watching path) cmd

withProgram :: (GL.Program -> StateT World IO ()) -> StateT World IO ()
withProgram cmd = 
    use shaderProgram >>= \case
        Nothing -> return ()
        Just program -> cmd program

updateShaderIfNeeded :: FilePath -> StateT World IO ()
updateShaderIfNeeded path = doIfUpdate $ do
    io (runExceptT (installShaders path)) >>= \case
        Left e -> io $ putStrLn $ "Error: " ++ show e
        Right program -> shaderProgram .= Just program

reshape :: (Int32, Int32) -> StateT World IO ()
reshape (w, h) = do
    withProgram $ \program -> io $ setShaderWindow program (w, h)
    GL.viewport GL.$= (GL.Position 0 0, GL.Size (i w) (i h))
    windowSize .= (w, h)

mouse :: (Int32, Int32) -> StateT World IO ()
mouse (x, y) = withProgram $ \program -> do
    (_, h) <- use windowSize
    io $ setShaderMouse program (x, h-y)

loop :: Options -> StateT World IO ()
loop options = do
    window <- use mainWindow
    interval <- realToFrac <$> (diffUTCTime <$> io getCurrentTime <*> use startTime)
    withProgram $ \program -> io $ setShaderTime program interval
    io $ render >> SDL.glSwapWindow window
    updateShaderIfNeeded (_shaderDirectory options)
    events <- io SDL.pollEvents
    quit <- mapM handleUIEvent events
    unless (or quit) $ loop options

handleKey :: SDL.Keysym -> StateT World IO Bool
handleKey (SDL.Keysym {SDL.keysymScancode = SDL.ScancodeEscape}) = return True
handleKey (SDL.Keysym { }) = return False

handlePayload :: SDL.EventPayload -> StateT World IO Bool
handlePayload (SDL.WindowResizedEvent
                            (SDL.WindowResizedEventData { SDL.windowResizedEventSize = V2 w h })) =
                            reshape (w, h) >> return False
handlePayload (SDL.MouseMotionEvent
                            (SDL.MouseMotionEventData { SDL.mouseMotionEventPos = P (V2 x y) })) =
        mouse (x, y) >> return False
handlePayload (SDL.KeyboardEvent
                            (SDL.KeyboardEventData { SDL.keyboardEventKeyMotion = SDL.Pressed, SDL.keyboardEventKeysym = k })) =
        handleKey k
handlePayload SDL.QuitEvent = return True
handlePayload _ = return False

handleUIEvent :: SDL.Event -> StateT World IO Bool
handleUIEvent SDL.Event { SDL.eventPayload = payload} = handlePayload payload

parse :: Options -> [String] -> Options
parse options [] = options
parse options ("-d" : path : args) = parse (options { _shaderDirectory = path }) args
parse options args = error ("Incomprehensible options " ++ unwords args)

main :: IO ()
main = do
    args <- getArgs
    let options = parse (Options { _shaderDirectory = "." }) args
    print options
     
    SDL.initialize [SDL.InitVideo]

    events <- atomically newTChan
    world <- init (_shaderDirectory options) events

    _ <- forkIO $ withManager $ \mgr -> do
        _ <- watchDir mgr (_shaderDirectory options) (const True) (atomically . writeTChan events)
        forever $ threadDelay 1000

    evalStateT (loop options) world
