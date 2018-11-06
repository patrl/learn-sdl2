{-# LANGUAGE OverloadedStrings  #-}

module LazyFoo16 where

import           SDL                            ( ($=) )
import qualified SDL
import qualified SDL.Font                      as SDLTtf
import           SDL.Vect
import           Control.Monad                  ( unless )
import           Foreign.C.Types                ( CInt )
import           Data.Word                      ( Word8 )
import qualified Data.Text                     as T

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

textureFromRenderedText
  :: SDL.Renderer -> SDLTtf.Font -> SDLTtf.Color -> T.Text -> IO SDL.Texture
textureFromRenderedText renderingContext loadedFont color textToRender = do
  textSurface <- SDLTtf.blended loadedFont color textToRender
  newTexture  <- SDL.createTextureFromSurface renderingContext textSurface
  SDL.freeSurface textSurface
  return newTexture

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  SDLTtf.initialize

  window <- SDL.createWindow
    "SDL Tutorial"
    SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
  SDL.showWindow window

  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  lazyFont <- SDLTtf.load "data/InputMono-Regular.ttf" 36

  texture  <- textureFromRenderedText
    renderer
    lazyFont
    (V4 0 0 0 0)
    "The quick brown fox jumps over the lazy dog"

  textureInfo <- SDL.queryTexture texture

  SDL.rendererDrawColor renderer $= clearColor


  let loop = do
        events <- map SDL.eventPayload <$> SDL.pollEvents
        let quit = SDL.QuitEvent `elem` events
        SDL.clear renderer
        SDL.copy renderer texture Nothing (Just $ SDL.Rectangle (P $ V2 0 0) (V2 (SDL.textureWidth textureInfo) (SDL.textureHeight textureInfo)))
        SDL.present renderer
        unless quit loop
  loop

  SDL.destroyRenderer renderer
  SDL.destroyTexture texture 
  SDL.destroyWindow window
  SDLTtf.free lazyFont
  SDLTtf.quit
  SDL.quit


