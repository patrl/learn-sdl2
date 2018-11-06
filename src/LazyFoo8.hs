{-# LANGUAGE OverloadedStrings  #-}

module LazyFoo8 where

import           SDL                            ( ($=) )
import qualified SDL
import           SDL.Vect
import           Control.Monad                  ( unless
                                                , when
                                                )
import           Control.Concurrent             ( threadDelay )
import           Foreign.C.Types                ( CInt )
import           Data.Word                      ( Word8 )
import           Data.Foldable                  ( for_ )

screenWidth, screenHeight :: CInt
screenWidth = 800
screenHeight = 600

yellow, blue, green, white, red, clearColor :: V4 Word8
green = V4 0 maxBound 0 maxBound
white = V4 maxBound maxBound maxBound maxBound
red = V4 maxBound 0 0 maxBound
blue = V4 0 0 maxBound maxBound
yellow = V4 maxBound maxBound 0 maxBound
clearColor = white

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  do
    renderQuality <- SDL.get SDL.HintRenderScaleQuality
    when (renderQuality /= SDL.ScaleLinear)
      $ putStrLn "Warning: Linear texture filtering not enabled!"

  window <- SDL.createWindow
    "SDL Tutorial"
    SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
  SDL.showWindow window

  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  SDL.rendererDrawColor renderer $= clearColor

  let loop = do
        events <- map SDL.eventPayload <$> SDL.pollEvents
        let quit = SDL.QuitEvent `elem` events

        SDL.rendererDrawColor renderer $= clearColor
        SDL.clear renderer
        SDL.rendererDrawColor renderer $= red
        SDL.fillRect
          renderer
          (Just $ SDL.Rectangle
            (P $ V2 (screenWidth `div` 4) (screenHeight `div` 4))
            (V2 (screenWidth `div` 2) (screenHeight `div` 2))
          )
        SDL.present renderer
        unless quit $ threadDelay 2000000
        SDL.rendererDrawColor renderer $= clearColor
        SDL.clear renderer
        SDL.rendererDrawColor renderer $= green
        SDL.drawRect
          renderer
          (Just $ SDL.Rectangle
            (P $ V2 (screenWidth `div` 6) (screenHeight `div` 6))
            (V2 ((screenWidth `div` 3) * 2) ((screenHeight `div` 3) * 2))
          )
        SDL.present renderer
        unless quit $ threadDelay 2000000
        SDL.rendererDrawColor renderer $= clearColor
        SDL.clear renderer
        SDL.rendererDrawColor renderer $= blue
        SDL.drawLine renderer
                     (P $ V2 0 (screenHeight `div` 2))
                     (P $ V2 screenWidth (screenHeight `div` 2))
        SDL.present renderer
        unless quit $ threadDelay 2000000
        SDL.rendererDrawColor renderer $= clearColor
        SDL.clear renderer
        SDL.rendererDrawColor renderer $= yellow
        for_ [0, 4 .. screenHeight] $ \i ->
          SDL.drawPoint renderer (P (V2 (screenWidth `div` 2) i))
        SDL.present renderer
        unless quit $ threadDelay 2000000
        unless quit loop
  loop

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit


