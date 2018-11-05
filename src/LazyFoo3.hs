{-# LANGUAGE OverloadedStrings  #-}

module Main where

import qualified SDL
import SDL.Vect
import Control.Monad (unless)
import           Control.Concurrent             ( threadDelay )

screenWidth = 800
screenHeight = 600

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  window <- SDL.createWindow
    "SDL Tutorial"
    SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }

  screenSurface <- SDL.getWindowSurface window

  helloWorld <- SDL.loadBMP "data/hello_world.bmp"

  let loop = do
        events <- SDL.pollEvents
        let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
        SDL.surfaceBlit helloWorld Nothing screenSurface Nothing
        SDL.updateWindowSurface window
        unless quit loop

  loop

  SDL.freeSurface helloWorld

  SDL.destroyWindow window

  SDL.quit
