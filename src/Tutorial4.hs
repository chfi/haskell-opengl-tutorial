{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C

import Data.Foldable (toList)

import Linear

import Foreign hiding (rotate)
import Control.Monad (when, forever)
import System.Exit

import Graphics.GL

import GHC.Float (double2Float)

import Shader

import qualified Graphics.UI.GLFW as GLFW


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

cube_colors :: [GLfloat]
cube_colors = [ 0.583,  0.771,  0.014
              , 0.609,  0.115,  0.436
              , 0.327,  0.483,  0.844
              , 0.822,  0.569,  0.201
              , 0.435,  0.602,  0.223
              , 0.310,  0.747,  0.185
              , 0.597,  0.770,  0.761
              , 0.559,  0.436,  0.730
              , 0.359,  0.583,  0.152
              , 0.483,  0.596,  0.789
              , 0.559,  0.861,  0.639
              , 0.195,  0.548,  0.859
              , 0.014,  0.184,  0.576
              , 0.771,  0.328,  0.970
              , 0.406,  0.615,  0.116
              , 0.676,  0.977,  0.133
              , 0.971,  0.572,  0.833
              , 0.140,  0.616,  0.489
              , 0.997,  0.513,  0.064
              , 0.945,  0.719,  0.592
              , 0.543,  0.021,  0.978
              , 0.279,  0.317,  0.505
              , 0.167,  0.620,  0.077
              , 0.347,  0.857,  0.137
              , 0.055,  0.953,  0.042
              , 0.714,  0.505,  0.345
              , 0.783,  0.290,  0.734
              , 0.722,  0.645,  0.174
              , 0.302,  0.455,  0.848
              , 0.225,  0.587,  0.040
              , 0.517,  0.713,  0.338
              , 0.053,  0.959,  0.120
              , 0.393,  0.621,  0.362
              , 0.673,  0.211,  0.457
              , 0.820,  0.883,  0.371
              , 0.982,  0.099,  0.879
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

  window <- GLFW.createWindow 1024 768 "Tutorial 04" Nothing Nothing
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

      programId <- loadShaders "shaders/tutorial4Vert.vert" "shaders/tutorial4Frag.frag"

      vertexBuffer <- bindBuffer cube_vertices
      colorBuffer <- bindBuffer cube_colors

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

        attribId <- getShaderAttrib programId "aPosition"
        colorId <- getShaderAttrib programId "aColor"

        glEnableVertexAttribArray (fromIntegral attribId)
        glBindBuffer GL_ARRAY_BUFFER vertexBuffer
        glVertexAttribPointer (fromIntegral attribId) 3 GL_FLOAT GL_FALSE 0 nullPtr

        glEnableVertexAttribArray (fromIntegral colorId)
        glBindBuffer GL_ARRAY_BUFFER colorBuffer
        glVertexAttribPointer (fromIntegral colorId) 3 GL_FLOAT GL_FALSE 0 nullPtr

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
