{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import qualified SDL
import Data.Maybe
import Data.Monoid
import           SDL.Vect
import           Control.Monad                  ( unless )
import Foreign.C.Types ( CInt )

screenWidth,screenHeight :: Foreign.C.Types.CInt
screenWidth = 800
screenHeight = 600

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  window <- SDL.createWindow
    "SDL Tutorial"
    SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }

  screenSurface <- SDL.getWindowSurface window

  [keyPressSurfaceDefault, keyPressSurfaceUp, keyPressSurfaceDown, keyPressSurfaceLeft, keyPressSurfaceRight] <-
    mapM (SDL.loadBMP . (++) "data/")
        ["press.bmp", "up.bmp", "down.bmp", "left.bmp", "right.bmp"]
  let keyPressSurfaces =
        [ keyPressSurfaceDefault
        , keyPressSurfaceUp
        , keyPressSurfaceDown
        , keyPressSurfaceLeft
        , keyPressSurfaceRight
        ]

  let loop oldSurface = do
        events <- map SDL.eventPayload <$> SDL.pollEvents
        let quit = SDL.QuitEvent `elem` events
            currentSurface =
              fromMaybe oldSurface $ getLast $
              foldMap (\case SDL.KeyboardEvent e
                               | SDL.keyboardEventKeyMotion e == SDL.Pressed ->
                                   case SDL.keysymKeycode (SDL.keyboardEventKeysym e) of
                                     SDL.KeycodeUp -> Last (Just keyPressSurfaceUp)
                                     SDL.KeycodeDown -> Last (Just keyPressSurfaceDown)
                                     SDL.KeycodeRight -> Last (Just keyPressSurfaceRight)
                                     SDL.KeycodeLeft -> Last (Just keyPressSurfaceLeft)
                                     _ -> mempty
                             _ -> mempty) events
        _ <- SDL.surfaceBlit currentSurface Nothing screenSurface Nothing
        SDL.updateWindowSurface window
        unless quit (loop currentSurface)

  loop keyPressSurfaceDefault

  mapM_ SDL.freeSurface keyPressSurfaces

  SDL.destroyWindow window

  SDL.quit
