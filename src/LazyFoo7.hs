{-# LANGUAGE OverloadedStrings  #-}

module LazyFoo7 where

import           SDL                            ( ($=) )
import qualified SDL
import           SDL.Vect
import           Control.Monad                  ( unless )
import           Foreign.C.Types                ( CInt )
import qualified SDL.Image                     as SDLImg

screenWidth, screenHeight :: Foreign.C.Types.CInt
screenWidth = 800
screenHeight = 600

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
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  _ <- SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound

  texture <- loadTexture "texture.png" renderer

  let loop = do
        events <- map SDL.eventPayload <$> SDL.pollEvents
        let quit = SDL.QuitEvent `elem` events
        SDL.clear renderer
        SDL.copy renderer texture Nothing Nothing
        SDL.present renderer
        unless quit loop

  loop

  SDL.destroyTexture texture

  SDL.destroyRenderer renderer

  SDL.destroyWindow window

  SDLImg.quit

  SDL.quit