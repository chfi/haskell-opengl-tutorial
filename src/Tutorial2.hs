module Main where


import Foreign
import Control.Monad (when, forever)
import System.Exit

import Graphics.GL

import Shader

import qualified Graphics.UI.GLFW as GLFW

main :: IO ()
main = do
  inited <- GLFW.init
  when (not inited) (die "Error in glfwInit")

  GLFW.windowHint $ GLFW.WindowHint'Samples 4
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
  GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
  GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core

  window <- GLFW.createWindow 1024 768 "Tutorial 02" Nothing Nothing
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

      programId <- loadShaders "shaders/tutorial2Vert.vert" "shaders/tutorial2Frag.frag"

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

        glUseProgram programId

        glEnableVertexAttribArray 0
        glBindBuffer GL_ARRAY_BUFFER vertexBuffer
        glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE 0 nullPtr

        glDrawArrays GL_TRIANGLES 0 3
        glDisableVertexAttribArray 0

        GLFW.swapBuffers win
        GLFW.pollEvents

        escDown <- (GLFW.KeyState'Pressed ==) <$> GLFW.getKey win GLFW.Key'Escape
        shdClose <- GLFW.windowShouldClose win

        when (escDown || shdClose) $ do
          glDeleteProgram programId
          GLFW.terminate
          exitSuccess
