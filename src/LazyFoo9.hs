{-# LANGUAGE OverloadedStrings  #-}

module LazyFoo9 where

import           SDL                            ( ($=) )
import qualified SDL
import qualified SDL.Image as SDLImg
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

loadTexture :: FilePath -> SDL.Renderer -> IO SDL.Texture
loadTexture path renderingContext = do
  loadedSurface <- SDLImg.load $ "data/" ++ path
  newTexture    <- SDL.createTextureFromSurface renderingContext loadedSurface
  SDL.freeSurface loadedSurface
  return newTexture


main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  window <- SDL.createWindow
    "SDL Tutorial"
    SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
  SDL.showWindow window

  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  SDL.rendererDrawColor renderer $= clearColor

  let topLeftViewportRect = SDL.Rectangle (P $ V2 0 0) (V2 (screenWidth `div` 2) (screenHeight `div` 2))
      topRightViewportRect = SDL.Rectangle (P $ V2 (screenWidth `div` 2) 0) (V2 (screenWidth `div` 2) (screenWidth `div` 2))
      bottomViewportRect = SDL.Rectangle (P $ V2 0 (screenHeight `div` 2)) (V2 screenWidth (screenHeight `div` 2))

  texture <- loadTexture "texture.png" renderer

  let loop = do
        events <- map SDL.eventPayload <$> SDL.pollEvents
        let quit = SDL.QuitEvent `elem` events
        SDL.clear renderer
        SDL.rendererViewport renderer $= return topLeftViewportRect
        SDL.copy renderer texture Nothing Nothing
        SDL.rendererViewport renderer $= return topRightViewportRect
        SDL.copy renderer texture Nothing Nothing
        SDL.rendererViewport renderer $= return bottomViewportRect
        SDL.copy renderer texture Nothing Nothing
        SDL.present renderer
        unless quit loop
  loop

  SDL.destroyTexture texture
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit


