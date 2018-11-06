{-# LANGUAGE OverloadedStrings  #-}

module LazyFoo10 where

import           SDL                            ( ($=) )
import qualified SDL
import qualified SDL.Image                     as SDLImg
import           SDL.Vect
import           Control.Monad                  ( unless
                                                )
import           Foreign.C.Types                ( CInt )
import           Data.Word                      ( Word8 )

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

loadForeground :: FilePath -> SDL.Renderer -> IO SDL.Texture
loadForeground path renderingContext = do
  loadedSurface <- SDLImg.load $ "data/" ++ path
  SDL.surfaceColorKey loadedSurface $= Just (V4 0 maxBound maxBound maxBound)
  newTexture <- SDL.createTextureFromSurface renderingContext loadedSurface
  SDL.freeSurface loadedSurface
  return newTexture

loadBackground :: FilePath -> SDL.Renderer -> IO SDL.Texture
loadBackground path renderingContext = do
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

  foreground     <- loadForeground "foo.png" renderer

  background     <- loadBackground "background.png" renderer

  foregroundInfo <- SDL.queryTexture foreground

  let foregroundWidth  = SDL.textureWidth foregroundInfo
      foregroundHeight = SDL.textureHeight foregroundInfo

  let loop = do
        events <- map SDL.eventPayload <$> SDL.pollEvents
        let quit = SDL.QuitEvent `elem` events
        SDL.clear renderer
        SDL.copy renderer background Nothing Nothing
        SDL.copy renderer foreground Nothing $ Just $ SDL.Rectangle
          (P $ V2 240 190)
          (V2 foregroundWidth foregroundHeight)
        SDL.present renderer
        unless quit loop
  loop

  SDL.destroyTexture foreground
  SDL.destroyTexture background
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit


