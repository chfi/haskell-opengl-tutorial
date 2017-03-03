{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C

import Data.Foldable (toList)
import Data.Vector.Storable (unsafeWith)

import Linear

import Codec.Picture
import Foreign hiding (rotate)
import Control.Monad (when, forever)
import System.Exit

import Data.WavefrontObj

import Graphics.GL

import GHC.Float (double2Float)

import Shader

import qualified Graphics.UI.GLFW as GLFW


loadTexture :: FilePath -> IO GLuint
loadTexture fp = alloca $ \txtPtr -> do
  Right (ImageRGB8 img) <- readImage fp
  unsafeWith (imageData img) $ \pxsPtr -> do
    glGenTextures 1 txtPtr
    glBindTexture GL_TEXTURE_2D =<< peek txtPtr
    glTexImage2D GL_TEXTURE_2D 0
      (fromIntegral GL_RGB)
      (fromIntegral $ imageWidth img) (fromIntegral $ imageHeight img)
      0 GL_RGB GL_UNSIGNED_BYTE pxsPtr
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral GL_NEAREST)
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral GL_NEAREST)
  peek txtPtr


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


bindBuffer :: Storable a => [a] -> IO GLuint
bindBuffer vs = alloca $ \ptr -> do
        glGenBuffers 1 ptr
        glBindBuffer GL_ARRAY_BUFFER =<< peek ptr
        withArray vs $ \arr -> do
          glBufferData
            GL_ARRAY_BUFFER
            (fromIntegral (sizeOf (head vs) * length vs))
            arr
            GL_STATIC_DRAW
        peek ptr


cube_vertices :: [GLfloat]
cube_vertices = [-1.0,-1.0,-1.0,
                 -1.0,-1.0, 1.0,
                 -1.0, 1.0, 1.0,
                  1.0, 1.0,-1.0,
                  -1.0,-1.0,-1.0,
                  -1.0, 1.0,-1.0,
                  1.0,-1.0, 1.0,
                  -1.0,-1.0,-1.0,
                  1.0,-1.0,-1.0,
                  1.0, 1.0,-1.0,
                  1.0,-1.0,-1.0,
                  -1.0,-1.0,-1.0,
                  -1.0,-1.0,-1.0,
                  -1.0, 1.0, 1.0,
                  -1.0, 1.0,-1.0,
                  1.0,-1.0, 1.0,
                  -1.0,-1.0, 1.0,
                  -1.0,-1.0,-1.0,
                  -1.0, 1.0, 1.0,
                  -1.0,-1.0, 1.0,
                  1.0,-1.0, 1.0,
                  1.0, 1.0, 1.0,
                  1.0,-1.0,-1.0,
                  1.0, 1.0,-1.0,
                  1.0,-1.0,-1.0,
                  1.0, 1.0, 1.0,
                  1.0,-1.0, 1.0,
                  1.0, 1.0, 1.0,
                  1.0, 1.0,-1.0,
                  -1.0, 1.0,-1.0,
                  1.0, 1.0, 1.0,
                  -1.0, 1.0,-1.0,
                  -1.0, 1.0, 1.0,
                  1.0, 1.0, 1.0,
                  -1.0, 1.0, 1.0,
                  1.0,-1.0, 1.0
                ]

cube_uv :: [GLfloat]
cube_uv = [ 0.000059, 1.0-0.000004
          , 0.000103, 1.0-0.336048
          , 0.335973, 1.0-0.335903
          , 1.000023, 1.0-0.000013
          , 0.667979, 1.0-0.335851
          , 0.999958, 1.0-0.336064
          , 0.667979, 1.0-0.335851
          , 0.336024, 1.0-0.671877
          , 0.667969, 1.0-0.671889
          , 1.000023, 1.0-0.000013
          , 0.668104, 1.0-0.000013
          , 0.667979, 1.0-0.335851
          , 0.000059, 1.0-0.000004
          , 0.335973, 1.0-0.335903
          , 0.336098, 1.0-0.000071
          , 0.667979, 1.0-0.335851
          , 0.335973, 1.0-0.335903
          , 0.336024, 1.0-0.671877
          , 1.000004, 1.0-0.671847
          , 0.999958, 1.0-0.336064
          , 0.667979, 1.0-0.335851
          , 0.668104, 1.0-0.000013
          , 0.335973, 1.0-0.335903
          , 0.667979, 1.0-0.335851
          , 0.335973, 1.0-0.335903
          , 0.668104, 1.0-0.000013
          , 0.336098, 1.0-0.000071
          , 0.000103, 1.0-0.336048
          , 0.000004, 1.0-0.671870
          , 0.336024, 1.0-0.671877
          , 0.000103, 1.0-0.336048
          , 0.336024, 1.0-0.671877
          , 0.335973, 1.0-0.335903
          , 0.667969, 1.0-0.671889
          , 1.000004, 1.0-0.671847
          , 0.667979, 1.0-0.335851
          ]


main :: IO ()
main = do
  inited <- GLFW.init
  when (not inited) (die "Error in glfwInit")

  GLFW.windowHint $ GLFW.WindowHint'Samples 4
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
  GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
  GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core

  window <- GLFW.createWindow 1024 768 "Tutorial 05" Nothing Nothing
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

      glClearColor 0 0 1.0 0

      glEnable GL_DEPTH_TEST

      glDepthFunc GL_LESS

      _ <- alloca $ \ptr -> do
        glGenVertexArrays 1 ptr
        glBindVertexArray =<< peek ptr
        peek ptr


      textureId <- loadTexture "resources/uvtemplate.tga"

      programId <- loadShaders "shaders/tutorial5Vert.vert" "shaders/tutorial5Frag.frag"



      vertexBuffer <- bindBuffer cube_vertices
      uvBuffer <- bindBuffer cube_uv
      -- colorBuffer <- bindBuffer cube_colors

      forever $ do
        glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

        time <- (maybe 0 (double2Float) <$> GLFW.getTime)

        let projection = perspective 1 1.25 0.1 100
        let view = lookAt (V3 4 3 3) (V3 0 0 0) (V3 0 1 0)
        let model = mkTransformation (axisAngle (V3 0 1 0) $ time * 1) (V3 0 0 0)
            mvp = projection !*! view !*! model

        glUseProgram programId

        matrixId <- getShaderUniform programId "mvp"

        withArray (concatMap toList (transpose mvp)) $ \mvpPointer -> do
          glUniformMatrix4fv (fromIntegral matrixId) 1 GL_FALSE mvpPointer

        attribId <- getShaderAttrib programId "vPosition"
        uvId <- getShaderAttrib programId "vUV"

        samplerId <- getShaderUniform programId "sampler"

        glActiveTexture GL_TEXTURE0
        glBindTexture GL_TEXTURE_2D textureId
        -- glUniform1i samplerId (fromIntegral textureId)

        glEnableVertexAttribArray (fromIntegral attribId)
        glBindBuffer GL_ARRAY_BUFFER vertexBuffer
        glVertexAttribPointer (fromIntegral attribId) 3 GL_FLOAT GL_FALSE 0 nullPtr

        glEnableVertexAttribArray (fromIntegral uvId)
        glBindBuffer GL_ARRAY_BUFFER uvBuffer
        glVertexAttribPointer (fromIntegral uvId) 2 GL_FLOAT GL_FALSE 0 nullPtr
        -- glEnableVertexAttribArray (fromIntegral colorId)
        -- glBindBuffer GL_ARRAY_BUFFER colorBuffer
        -- glVertexAttribPointer (fromIntegral colorId) 3 GL_FLOAT GL_FALSE 0 nullPtr

        glDrawArrays GL_TRIANGLES 0 (12*3)
        glDisableVertexAttribArray (fromIntegral attribId)

        GLFW.swapBuffers win
        GLFW.pollEvents

        escDown <- (GLFW.KeyState'Pressed ==) <$> GLFW.getKey win GLFW.Key'Escape
        shdClose <- GLFW.windowShouldClose win

        when (escDown || shdClose) $ do
          glDeleteProgram programId
          GLFW.terminate
          exitSuccess
