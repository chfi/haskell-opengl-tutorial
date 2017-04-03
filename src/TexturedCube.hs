{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C

import Data.Foldable (toList)
import Data.Vector.Storable (unsafeWith)
-- import Data.Vector (toList)
import Data.IORef

import Linear

import Codec.Picture
import Codec.Wavefront (WavefrontOBJ(..))
import qualified Codec.Wavefront as OBJ
import Foreign hiding (rotate)
import Control.Lens
import Control.Monad (when, forever, guard)
import System.Exit

import Graphics.GL

import GHC.Float (double2Float)
import System.Random (randomRIO)
import Shader

import Graphics.UI.GLFW (Window, Key, KeyState, ModifierKeys)
import qualified Graphics.UI.GLFW as GLFW


data PlayerState = PlayerState
  { _pos :: V3 GLfloat
  , _posD :: V3 GLfloat
  , _camF :: V3 GLfloat
  , _camU :: V3 GLfloat
  , _hAngD :: GLfloat
  , _vAngD :: GLfloat
  } deriving (Eq, Ord, Show)


makeLenses ''PlayerState

glUintSize :: Int
glUintSize = fromIntegral $ sizeOf (undefined :: GLuint)
glFloatSize :: Int
glFloatSize = fromIntegral $ sizeOf (undefined :: GLfloat)


getShaderAttrib :: GLuint -> ByteString -> IO GLint
getShaderAttrib prog attribName = do
  location <- C.useAsCString attribName $ \attribNameCString ->
    glGetAttribLocation prog attribNameCString
  when (location == -1) $ error $ "Couldn't bind attrib: " ++ C.unpack attribName
  pure location

getShaderUniform :: GLuint -> ByteString -> IO GLint
getShaderUniform prog uniformName = do
  location <- C.useAsCString uniformName $ \uniformNameCString ->
    glGetUniformLocation prog uniformNameCString
  when (location == -1) $ error $ "Couldn't bind uniform: " ++ C.unpack uniformName
  pure location



rectVertices :: [GLfloat]
rectVertices =
  [
    -- positions       -- colors        -- texture coordinates
     0.5,  0.5, 0.0,   1.0, 0.0, 0.0,   1.0, 1.0,   -- Top Right
     0.5, -0.5, 0.0,   0.0, 1.0, 0.0,   1.0, 0.0,   -- Bottom Right
    -0.5, -0.5, 0.0,   0.0, 0.0, 1.0,   0.0, 0.0,   -- Bottom Left
    -0.5,  0.5, 0.0,   1.0, 1.0, 0.0,   0.0, 1.0    -- Top Left
  ]


rectIndices :: [GLuint]
rectIndices =
  [
    0, 1, 3, -- First Triangle
    1, 2, 3  -- Second Triangle
  ]


modelPath :: String
modelPath = "resources/teapot.obj"


mainKeyCallback :: IORef PlayerState -> Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
mainKeyCallback psref w k i ks mk = do
  let del = if ks == GLFW.KeyState'Pressed ||
               ks == GLFW.KeyState'Repeating then 1.0 else 0.0
  case k of
    GLFW.Key'Escape -> GLFW.setWindowShouldClose w True
    GLFW.Key'Left -> do
      modifyIORef psref $ \ps ->
        let c = cross (_camF ps) (_camU ps)
        in ps & posD .~ c * (-0.05) * pure del
    GLFW.Key'Right -> do
      modifyIORef psref $ \ps ->
        let c = cross (_camF ps) (_camU ps)
        in ps & posD .~ c *   0.05  * pure del
    GLFW.Key'Up -> do
      modifyIORef psref $ \ps ->
        ps & posD .~ view camF ps * pure del *   0.1
    GLFW.Key'Down -> do
      modifyIORef psref $ \ps ->
        ps & posD .~ view camF ps * pure del * (-0.1)
    _ -> pure ()


mainCursorCallback :: IORef (Double, Double)
                   -> IORef PlayerState -> Window -> Double -> Double -> IO ()
mainCursorCallback mref psref _ x y = do
  (lx,ly) <- readIORef mref
  let dx = x - lx
      dy = y - ly
  writeIORef mref (x, y)
  modifyIORef psref $ hAngD .~ double2Float dx * (-0.01)
  modifyIORef psref $ vAngD .~ double2Float dy * (-0.01)


main :: IO ()
main = do
  inited <- GLFW.init
  when (not inited) (die "Error in glfwInit")

  GLFW.windowHint $ GLFW.WindowHint'Samples 4
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
  GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
  GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core

  player <- newIORef $ PlayerState (V3 0 0 0) 0 (V3 0 0 1) (V3 0 1 0) 0 0
  mouse <- newIORef (0,0)

  window <- GLFW.createWindow 1024 768 "GL stuff" Nothing Nothing
  case window of
    Nothing -> do
      putStrLn "Failed to open GLFW window"
      _ <- getChar
      GLFW.terminate
      exitFailure
    Just win -> do
      GLFW.makeContextCurrent window
      (w,h) <- GLFW.getFramebufferSize win
      glViewport 0 0 (fromIntegral w) (fromIntegral h)

      GLFW.setStickyKeysInputMode win GLFW.StickyKeysInputMode'Enabled
      GLFW.setCursorInputMode win GLFW.CursorInputMode'Disabled

      GLFW.setKeyCallback win (Just $ mainKeyCallback player)
      GLFW.setCursorPosCallback win (Just $ mainCursorCallback mouse player)

      glClearColor 0 0 1.0 0

      glEnable GL_DEPTH_TEST

      glDepthFunc GL_LESS

      -- build & compile shader
      program <- loadShaders "shaders/textures.vert" "shaders/textures.frag"

      teaShader <- loadShaders "shaders/teapot.vert" "shaders/teapot.frag"

      -- create cube VAO
      vao <- alloca $ \vaoPtr -> glGenVertexArrays 1 vaoPtr >> peek vaoPtr
      -- create VBO & EBO
      vbo <- alloca $ \vboPtr -> glGenBuffers 1 vboPtr >> peek vboPtr
      ebo <- alloca $ \eboPtr -> glGenBuffers 1 eboPtr >> peek eboPtr


      teaVao <- alloca $ \vaoPtr -> glGenVertexArrays 1 vaoPtr >> peek vaoPtr
      -- create VBO & EBO
      teaVbo <- alloca $ \vboPtr -> glGenBuffers 1 vboPtr >> peek vboPtr
      teaEbo <- alloca $ \eboPtr -> glGenBuffers 1 eboPtr >> peek eboPtr

      -- bind VAO
      glBindVertexArray vao
      -- bind & load vertices
      glBindBuffer GL_ARRAY_BUFFER vbo
      withArray cubeVertices $ \arr ->
        glBufferData GL_ARRAY_BUFFER
          (fromIntegral $ glFloatSize * length cubeVertices) arr GL_STATIC_DRAW



      -- set attrib pointers
      let size = sizeOf (undefined :: GLfloat)
          stride = fromIntegral $ 5 * size
      glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE stride nullPtr
      glEnableVertexAttribArray 0
      glVertexAttribPointer 2 2 GL_FLOAT GL_FALSE stride (nullPtr `plusPtr` (3*size))
      glEnableVertexAttribArray 2
      -- glVertexAttribPointer 2 2 GL_FLOAT GL_FALSE stride (nullPtr `plusPtr` (6*size))
      -- glEnableVertexAttribArray 2

      -- unbind VAO
      glBindVertexArray 0


      -- load model (partial because fuck it)
      glBindVertexArray teaVao
      glBindBuffer GL_ARRAY_BUFFER teaVbo

      Right obj <- OBJ.fromFile modelPath
      withArray ((\(OBJ.Location x y z _) -> [x, y, z]) `concatMap` toList (objLocations obj)) $ \arr -> do
      -- withArray (pointLocIndex . elValue <$> toList (objPoints obj)) $ \arr ->
        -- let indices = objLocations obj
        --     loc2List (Location x y z _) = [x,y,z]
            -- verts = !!
        glBufferData GL_ARRAY_BUFFER
          (fromIntegral $ glFloatSize * length (objLocations obj) * 3) arr GL_STATIC_DRAW



      glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE (fromIntegral $ 3 * size) nullPtr
      glEnableVertexAttribArray 0
      glBindVertexArray 0


      -- create & bind texture
      texture <- alloca $ \txtPtr -> glGenTextures 1 txtPtr >> peek txtPtr
      glBindTexture GL_TEXTURE_2D texture

      -- set texture params
      glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S (fromIntegral GL_REPEAT)
      glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T (fromIntegral GL_REPEAT)
      -- // Set texture filtering parameters
      glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral GL_LINEAR)
      glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral GL_LINEAR)

      -- load image & create texture image
      Right x <- readJpeg "resources/container.jpg"
      let img = convertRGB8 x
      unsafeWith (imageData img) $ \imgPtr -> do
        glTexImage2D GL_TEXTURE_2D 0 (fromIntegral GL_RGB)
          (fromIntegral $ imageWidth img) (fromIntegral $ imageHeight img)
          0 GL_RGB GL_UNSIGNED_BYTE
          imgPtr
      -- generate mipmap
        glGenerateMipmap GL_TEXTURE_2D

      -- unbind texture
      glBindTexture GL_TEXTURE_2D 0



      forever $ do
        GLFW.pollEvents

        glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

        time <- (maybe 0 (double2Float) <$> GLFW.getTime)

        -- update player state
        p <- readIORef player
        let r = cross (_camF p) (_camU p)
        let p' = p { _pos = (_pos p) + (_posD p)
                   -- , _camF = rotate (axisAngle r (_vAngD p)) $
                   --             rotate (axisAngle (_camU p) (_hAngD p)) (_camF p)
                   , _camF = rotate (axisAngle (_camU p) (_hAngD p)) $
                               rotate (axisAngle r (_vAngD p)) (_camF p)
                   , _hAngD = 0
                   , _vAngD = 0
                   }
        writeIORef player p'

        let projection = perspective 1 1.25 0.1 100
        let view = lookAt (_pos p) (_pos p + _camF p) (_camU p)

        pMat <- getShaderUniform program "projection"
        vMat <- getShaderUniform program "view"
        mMat <- getShaderUniform program "model"

        glBindTexture GL_TEXTURE_2D texture

        glUseProgram program

        setUniformMatrix4fv projection pMat
        setUniformMatrix4fv view vMat

        glBindVertexArray vao

        mapM_ (\(v,i) -> do
                  let ang = normalize v
                  let s = 2 * sin (time * i * 0.1)
                  let scale = V4 (V4 s 0 0 0) (V4 0 s 0 0 ) (V4 0 0 s 0) (V4 0 0 0 1)
                  let model = (mkTransformation (axisAngle ang (i*time*0.1)) v) !*! scale
                  setUniformMatrix4fv model mMat
                  glDrawArrays GL_TRIANGLES 0 36

                  ) $ zip cubePositions [1..]
        glBindVertexArray 0

        glUseProgram teaShader
        glBindVertexArray teaVao
        let model = identity :: M44 GLfloat
        pTea <- getShaderUniform teaShader "projection"
        vTea <- getShaderUniform teaShader "view"
        mTea <- getShaderUniform teaShader "model"
        setUniformMatrix4fv projection pTea
        setUniformMatrix4fv view vTea
        setUniformMatrix4fv model mTea
        glDrawArrays GL_LINE_LOOP 0 (fromIntegral $ length (objLocations obj))
        glBindVertexArray 0


        GLFW.swapBuffers win

        shdClose <- GLFW.windowShouldClose win

        when shdClose $ do
          glDeleteProgram program
          with vbo $ \ptr -> glDeleteBuffers 1 ptr
          with ebo $ \ptr -> glDeleteBuffers 1 ptr

          GLFW.terminate
          exitSuccess



randUnitVector :: IO (V3 GLfloat)
randUnitVector = do
  x <- randomRIO (0,1)
  y <- randomRIO (0,1)
  z <- randomRIO (0,1)
  pure $ normalize $ V3 x y z


setUniformMatrix4fv mat uni = withArray (concatMap toList (transpose mat)) $ \matPtr -> do
  glUniformMatrix4fv (fromIntegral uni) 1 GL_FALSE matPtr


cubePositions :: [V3 GLfloat]
cubePositions =
  [
    V3  0.0  0.0  0.0,
    V3  2.0  5.0 (-15.0),
    V3 (-1.5) (-2.2) (-2.5),
    V3 (-3.8) (-2.0) (-12.3),
    V3  2.4 (-0.4) (-3.5),
    V3 (-1.7)  3.0 (-7.5),
    V3  1.3 (-2.0) (-2.5),
    V3  1.5  2.0 (-2.5),
    V3  1.5  0.2 (-1.5),
    V3 (-1.3)  1.0 (-1.5)
  ]


cubeVertices :: [GLfloat]
cubeVertices =
  [
    -0.5, -0.5, -0.5,  0.0, 0.0,
     0.5, -0.5, -0.5,  1.0, 0.0,
     0.5,  0.5, -0.5,  1.0, 1.0,
     0.5,  0.5, -0.5,  1.0, 1.0,
    -0.5,  0.5, -0.5,  0.0, 1.0,
    -0.5, -0.5, -0.5,  0.0, 0.0,

    -0.5, -0.5,  0.5,  0.0, 0.0,
     0.5, -0.5,  0.5,  1.0, 0.0,
     0.5,  0.5,  0.5,  1.0, 1.0,
     0.5,  0.5,  0.5,  1.0, 1.0,
    -0.5,  0.5,  0.5,  0.0, 1.0,
    -0.5, -0.5,  0.5,  0.0, 0.0,

    -0.5,  0.5,  0.5,  1.0, 0.0,
    -0.5,  0.5, -0.5,  1.0, 1.0,
    -0.5, -0.5, -0.5,  0.0, 1.0,
    -0.5, -0.5, -0.5,  0.0, 1.0,
    -0.5, -0.5,  0.5,  0.0, 0.0,
    -0.5,  0.5,  0.5,  1.0, 0.0,

     0.5,  0.5,  0.5,  1.0, 0.0,
     0.5,  0.5, -0.5,  1.0, 1.0,
     0.5, -0.5, -0.5,  0.0, 1.0,
     0.5, -0.5, -0.5,  0.0, 1.0,
     0.5, -0.5,  0.5,  0.0, 0.0,
     0.5,  0.5,  0.5,  1.0, 0.0,

    -0.5, -0.5, -0.5,  0.0, 1.0,
     0.5, -0.5, -0.5,  1.0, 1.0,
     0.5, -0.5,  0.5,  1.0, 0.0,
     0.5, -0.5,  0.5,  1.0, 0.0,
    -0.5, -0.5,  0.5,  0.0, 0.0,
    -0.5, -0.5, -0.5,  0.0, 1.0,

    -0.5,  0.5, -0.5,  0.0, 1.0,
     0.5,  0.5, -0.5,  1.0, 1.0,
     0.5,  0.5,  0.5,  1.0, 0.0,
     0.5,  0.5,  0.5,  1.0, 0.0,
    -0.5,  0.5,  0.5,  0.0, 0.0,
    -0.5,  0.5, -0.5,  0.0, 1.0
  ]
