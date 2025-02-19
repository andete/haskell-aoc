module Main where

import SDL
import Control.Monad (unless)
import Data.Text (pack)

main :: IO ()
main = do
  initializeAll
  window <- createWindow (pack "My SDL Application") defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer
  destroyWindow window

appLoop :: Renderer -> IO ()
appLoop renderer = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  rendererDrawColor renderer $= SDL.V4 0 0 255 255
  clear renderer
  present renderer
  unless qPressed (appLoop renderer)
