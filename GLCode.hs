{-# LANGUAGE OverloadedStrings #-}

module GLCode where

import Control.Monad.Except
import Graphics.Rendering.OpenGL
import Foreign.Marshal.Array
import qualified Data.ByteString as B
import Control.Exception
import System.FilePath
import Data.Int

io :: MonadIO m => IO a -> m a
io = liftIO

i :: (Num b, Integral a) => a -> b
i = fromIntegral

preamble :: B.ByteString
preamble = "\
    \#version 120\n\
    \uniform vec2 iResolution;\n\
    \uniform vec2 iMouse;\n\
    \uniform float iTime;\n\
    \#line 0\n"

postamble :: B.ByteString
postamble = "\
    \\n\
    \void main() {\n\
    \    vec4 color;\n\
    \    mainImage(color, gl_FragCoord.xy);\n\
    \    gl_FragColor = color;\n\
    \}"

data ShaderInfo = ShaderInfo ShaderType FilePath
    deriving (Eq, Ord, Show)

compileAndCheck :: Shader -> ExceptT String IO ()
compileAndCheck = checked compileShader compileStatus shaderInfoLog "compile"

preprocess :: ShaderType -> B.ByteString -> B.ByteString
preprocess VertexShader src = src
preprocess FragmentShader src = preamble `B.append` src `B.append` postamble
preprocess _ _ = error "Unknown shader type"

liftCatch :: (IOException -> e) -> IO a -> ExceptT e IO a
liftCatch f m = ExceptT $ liftM (either (Left . f) Right) (try m)

loadCompileAttach :: Program -> [ShaderInfo] -> ExceptT String IO ()
loadCompileAttach _ [] = return ()
loadCompileAttach program (ShaderInfo shType source : infos) = ExceptT $
   createShader shType `bracketOnError` deleteObjectName $ \shader -> runExceptT $ do
      src <- liftCatch show (B.readFile source)
      shaderSourceBS shader $= preprocess shType src
      compileAndCheck shader
      io $ attachShader program shader
      loadCompileAttach program infos

checked :: (t -> IO ())
        -> (t -> GettableStateVar Bool)
        -> (t -> GettableStateVar String)
        -> String
        -> t
        -> ExceptT String IO ()
checked action getStatus getInfoLog message object = do
    io $ action object
    ok <- get (getStatus object)
    unless ok $ do
        infoLog <- get (getInfoLog object)
        throwError (message ++ " log: " ++ infoLog)

linkAndCheck :: Program -> ExceptT String IO ()
linkAndCheck = checked linkProgram linkStatus programInfoLog "link"

loadShaders :: [ShaderInfo] -> ExceptT String IO Program
loadShaders infos = ExceptT $
   createProgram `bracketOnError` deleteObjectName $ \program -> runExceptT $ do
      loadCompileAttach program infos
      linkAndCheck program
      return program

installShaders :: FilePath -> ExceptT String IO Program
installShaders path = do
    program <- loadShaders [
        ShaderInfo VertexShader (joinPath [path, "litterbox.vert"]),
        ShaderInfo FragmentShader (joinPath [path, "litterbox.frag"])]

    currentProgram $= Just program
    attribLocation program "vPosition" $= AttribLocation 0
    io $ setShaderWindow program (512, 512)

    return program

vertices :: [Vertex2 GLfloat]
vertices = [
    Vertex2 (-1.00) (-1.00),  -- Triangle 1
    Vertex2   1.00  (-1.00),
    Vertex2 (-1.00)   1.00,
    Vertex2   1.00  (-1.00),  -- Triangle 2
    Vertex2   1.00    1.00,
    Vertex2 (-1.00)   1.00]

setShaderTime :: Program -> Float -> IO ()
setShaderTime program time = do
    loc <- uniformLocation program "iTime"
    uniform loc $= time

setShaderWindow :: Program -> (Int32, Int32) -> IO ()
setShaderWindow program (w, h) = do
    loc <- uniformLocation program "iResolution"
    uniform loc $= Vector2 (i w) (i h::Float)

setShaderMouse :: Program -> (Int32, Int32) -> IO ()
setShaderMouse program (w, h) = do
    loc <- uniformLocation program "iMouse"
    uniform loc $= Vector2 (i w) (i h::Float)

render :: IO ()
render = do
    clearColor $= Color4 0.3 0.3 0.3 1
    clear [ColorBuffer]
    vertexAttribArray (AttribLocation 0) $= Enabled
    withArray vertices $ \ptr ->
        vertexAttribPointer (AttribLocation 0) $=
          (ToFloat, VertexArrayDescriptor 2 Float 0 ptr)
    drawArrays Triangles 0 6
    vertexAttribArray (AttribLocation 0) $= Disabled
    flush
