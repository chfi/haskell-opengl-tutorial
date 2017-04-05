{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C

import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid ((<>), mempty)
import Data.Foldable (toList)
import qualified Data.Vector.Storable as VS
import Data.Vector.Storable (unsafeWith)
import qualified Data.Vector as V
import Data.Vector (convert, Vector, snoc, cons)
import Data.IORef

import Linear

import Codec.Picture
import Codec.Wavefront (WavefrontOBJ(..), Face(..), FaceIndex(..))
import qualified Codec.Wavefront as OBJ
import Foreign hiding (rotate)
import Control.Lens hiding (snoc, cons)
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




-- currently just positions

facesToIndices :: V.Vector Face -> V.Vector GLuint
facesToIndices fs = V.fromList $ concatMap
  (\(Face a b c _) ->
      pred . fromIntegral . OBJ.faceLocIndex <$> [a,b,c]
      ) fs


facesToTexIndices :: V.Vector Face -> V.Vector GLuint
facesToTexIndices fs = V.fromList $ concatMap
  (\(Face a b c _) ->
      catMaybes $ ((fmap (pred . fromIntegral)) . OBJ.faceTexCoordIndex) <$> [a,b,c]
      ) fs


facesToNorIndices :: V.Vector Face -> V.Vector GLuint
facesToNorIndices fs = V.fromList $ concatMap
  (\(Face a b c _) ->
      catMaybes $ ((fmap (pred . fromIntegral)) . OBJ.faceNorIndex) <$> [a,b,c]
      ) fs


locationToList :: OBJ.Location -> [GLfloat]
locationToList (OBJ.Location x y z _) = [x,y,z]

texCoordToList :: OBJ.TexCoord -> [GLfloat]
  -- `1 - v` because the opengl coordinate system is different from OBJ's UV coords
texCoordToList (OBJ.TexCoord u v _) = [u, 1-v]


faceToVerts :: V.Vector OBJ.Location -> V.Vector OBJ.TexCoord -> Face -> [GLfloat]
faceToVerts vs ts (Face (FaceIndex v1 t1 _)
                        (FaceIndex v2 t2 _)
                        (FaceIndex v3 t3 _) _) =
  let helpT t = fromMaybe [0,0] $ t >>= \x -> texCoordToList <$> (ts V.!? (x-1))
      helpV v = locationToList (vs V.! (v-1))
  -- in concat [ (concatMap helpV [v1,v2,v3])
  --           , (concatMap helpT [t1,t2,t3])
  --           ]
      [t1', t2', t3'] = helpT <$> [t1,t2,t3]
      [v1', v2', v3'] = helpV <$> [v1,v2,v3]
  in concat [v1', t1', v2', t2', v3', t3']

-- create vector of vertices, then add each face vertex to it

facesToBuffer :: V.Vector OBJ.Location -> V.Vector OBJ.TexCoord -> V.Vector Face -> V.Vector GLfloat
facesToBuffer vs ts fs = V.fromList $ concatMap (faceToVerts vs ts) fs


  -- TODO make this function total
loadObj :: String -> IO (V.Vector GLfloat)
loadObj fp = do
  Right obj <- OBJ.fromFile fp
  pure $ facesToBuffer (objLocations obj) (objTexCoords obj) (OBJ.elValue <$> objFaces obj)

-- TODO should have functions that take care of binding & unbinding VAOs etc.
objLoadBuffer :: WavefrontOBJ -> GLuint -> IO ()
objLoadBuffer obj vbo = do
  let bfr = facesToBuffer (objLocations obj) (objTexCoords obj) (OBJ.elValue <$> objFaces obj)
  glBindBuffer GL_ARRAY_BUFFER vbo
  withArray (V.toList bfr) $ \arr -> do
    glBufferData GL_ARRAY_BUFFER
      (fromIntegral $ glFloatSize * length bfr) arr GL_STATIC_DRAW
  glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE (fromIntegral $ 5 * glFloatSize) nullPtr
  glEnableVertexAttribArray 0

  glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE (fromIntegral $ 5 * glFloatSize)
    (nullPtr `plusPtr` (3 * glFloatSize))
  glEnableVertexAttribArray 1



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

      glClearColor 0 0 0 0

      glEnable GL_DEPTH_TEST

      glDepthFunc GL_LESS

      -- build & compile shader
      program <- loadShaders "shaders/textures.vert" "shaders/textures.frag"


      teaVao <- alloca $ \vaoPtr -> glGenVertexArrays 1 vaoPtr >> peek vaoPtr
      -- create VBO & EBO
      teaVbo <- alloca $ \vboPtr -> glGenBuffers 1 vboPtr >> peek vboPtr
      teaEbo <- alloca $ \eboPtr -> glGenBuffers 1 eboPtr >> peek eboPtr



      glBindVertexArray teaVao

      -- load model (partial because fuck it)
      Right obj <- OBJ.fromFile "resources/capsule.obj"
      objLoadBuffer obj teaVbo

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
      Right x <- readJpeg "resources/capsule0.jpg"
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
                   , _camF = rotate (axisAngle (_camU p) (_hAngD p)) $
                               rotate (axisAngle r (_vAngD p)) (_camF p)
                   , _hAngD = 0
                   , _vAngD = 0
                   }
        writeIORef player p'

        let projection = perspective 1 1.25 0.1 100
        let view = lookAt (_pos p) (_pos p + _camF p) (_camU p)

        glBindTexture GL_TEXTURE_2D texture

        glUseProgram program


        pTea <- getShaderUniform program "projection"
        vTea <- getShaderUniform program "view"
        mTea <- getShaderUniform program "model"
        setUniformMatrix4fv projection pTea
        setUniformMatrix4fv view vTea

        glBindVertexArray teaVao


        mapM_ (\(v,i) -> do
                  let ang = normalize v
                  let s = 2 * sin (time * i * 0.1)
                  let scale = V4 (V4 s 0 0 0) (V4 0 s 0 0 ) (V4 0 0 s 0) (V4 0 0 0 1)
                  let model = (mkTransformation (axisAngle ang (i*time*0.1)) v) !*! scale
                  setUniformMatrix4fv model mTea
                  glDrawArrays GL_TRIANGLES 0 (fromIntegral $ length (objFaces obj) * 3)

                  ) $ zip cubePositions [1..]

        glBindVertexArray 0


        GLFW.swapBuffers win

        shdClose <- GLFW.windowShouldClose win

        when shdClose $ do
          glDeleteProgram program
          with teaVbo $ \ptr -> glDeleteBuffers 1 ptr
          with teaEbo $ \ptr -> glDeleteBuffers 1 ptr

          GLFW.terminate
          exitSuccess



randUnitVector :: IO (V3 GLfloat)
randUnitVector = do
  x <- randomRIO (-1,1)
  y <- randomRIO (-1,1)
  z <- randomRIO (-1,1)
  pure $ normalize $ V3 x y z


setUniformMatrix4fv :: Integral a => M44 GLfloat -> a -> IO ()
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
