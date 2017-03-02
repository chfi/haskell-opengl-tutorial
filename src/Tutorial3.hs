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

main :: IO ()
main = do
  inited <- GLFW.init
  when (not inited) (die "Error in glfwInit")

  GLFW.windowHint $ GLFW.WindowHint'Samples 4
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
  GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
  GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core

  window <- GLFW.createWindow 1024 768 "Tutorial 03" Nothing Nothing
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

      _ <- alloca $ \ptr -> do
        glGenVertexArrays 1 ptr
        glBindVertexArray =<< peek ptr
        peek ptr

      programId <- loadShaders "shaders/tutorial3Vert.vert" "shaders/tutorial2Frag.frag"

      let g_vertex_buffer_data = [ -1, -1, 0
                                 ,  1, -1, 0
                                 ,  0,  1, 0
                                 ] :: [GLfloat]

      vertexBuffer <- alloca $ \ptr -> do
        glGenBuffers 1 ptr
        glBindBuffer GL_ARRAY_BUFFER =<< peek ptr
        withArray g_vertex_buffer_data $ \arr -> do
          -- copy vertices to buffer in GPU memory
          glBufferData
            GL_ARRAY_BUFFER
            (fromIntegral (sizeOf (undefined :: GLfloat) * length g_vertex_buffer_data))
            arr
            GL_STATIC_DRAW
        peek ptr



      forever $ do
        glClear GL_COLOR_BUFFER_BIT


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

        glEnableVertexAttribArray (fromIntegral attribId)
        glBindBuffer GL_ARRAY_BUFFER vertexBuffer
        glVertexAttribPointer (fromIntegral attribId) 3 GL_FLOAT GL_FALSE 0 nullPtr

        glDrawArrays GL_TRIANGLES 0 3
        glDisableVertexAttribArray (fromIntegral attribId)

        GLFW.swapBuffers win
        GLFW.pollEvents

        escDown <- (GLFW.KeyState'Pressed ==) <$> GLFW.getKey win GLFW.Key'Escape
        shdClose <- GLFW.windowShouldClose win

        when (escDown || shdClose) $ do
          glDeleteProgram programId
          GLFW.terminate
          exitSuccess
