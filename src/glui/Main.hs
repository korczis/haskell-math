{-# LANGUAGE FlexibleContexts #-}

-- | See http://yannesposito.com/Scratch/en/blog/Haskell-OpenGL-Mandelbrot/

module Main  where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef

import System.Environment (
    getArgs
  )

-- | Main entry point
main :: IO ()
main = do
  -- GLUT need to be initialized
  (progname,_) <- getArgsAndInitialize
  -- We will use the double buffered mode (GL constraint)
  initialDisplayMode $= [DoubleBuffered]
  -- We create a window with some title
  createWindow "OpenGL UI"
  -- Each time we will need to update the display
  -- we will call the function 'display'
  displayCallback $= display
  -- We enter the main loop
  mainLoop

display = do
  clear [ColorBuffer] -- make the window black
  loadIdentity -- reset any transformation
  -- preservingMatrix drawMandelbrot
  swapBuffers -- refresh screen
