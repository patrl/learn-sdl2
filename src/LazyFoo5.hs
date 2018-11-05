{-# LANGUAGE OverloadedStrings  #-}

module LazyFoo5 where

import qualified SDL
import           SDL.Vect
import           Control.Monad                  ( unless )
import Foreign.C.Types ( CInt )

screenWidth,screenHeight :: Foreign.C.Types.CInt
screenWidth = 800
screenHeight = 600

loadAndOptimize :: FilePath -> SDL.Surface -> IO SDL.Surface
loadAndOptimize path surfaceToOptimizeFor = do
  loadedSurface <- SDL.loadBMP $ "data/" ++ path
  formatToOptimizeFor <- SDL.surfaceFormat surfaceToOptimizeFor
  convertedSurface <- SDL.convertSurface loadedSurface formatToOptimizeFor
  SDL.freeSurface loadedSurface
  return convertedSurface


main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  window <- SDL.createWindow
    "SDL Tutorial"
    SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }

  screenSurface <- SDL.getWindowSurface window
  convertedSurface <- loadAndOptimize "hello_world.bmp" screenSurface

  let loop = do
        events <- map SDL.eventPayload <$> SDL.pollEvents
        let quit = SDL.QuitEvent `elem` events
        SDL.surfaceBlitScaled convertedSurface Nothing screenSurface Nothing
        SDL.updateWindowSurface window
        unless quit loop

  loop

  SDL.freeSurface convertedSurface

  SDL.destroyWindow window

  SDL.quit


