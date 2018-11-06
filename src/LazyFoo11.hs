{-# LANGUAGE OverloadedStrings  #-}

module LazyFoo11 where

import           SDL                            ( ($=) )
import qualified SDL
import qualified SDL.Image                     as SDLImg
import           SDL.Vect
import           Control.Monad                  ( unless )
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

data TextureWithClips = TextureWithClips {
  texture :: SDL.Texture,
  topLeftRect :: SDL.Rectangle CInt,
  topRightRect :: SDL.Rectangle CInt,
  bottomLeftRect :: SDL.Rectangle CInt,
  bottomRightRect :: SDL.Rectangle CInt
                                            }

textureWithClips txture clip1 clip2 clip3 clip4 = TextureWithClips
  { texture         = txture
  , topLeftRect     = clip1
  , topRightRect    = clip2
  , bottomLeftRect  = clip3
  , bottomRightRect = clip4
  }

loadTextureAndClips :: FilePath -> SDL.Renderer -> IO TextureWithClips
loadTextureAndClips path renderingContext = do
  loadedSurface <- SDLImg.load $ "data/" ++ path
  SDL.surfaceColorKey loadedSurface $= Just (V4 0 maxBound maxBound maxBound)
  newTexture    <- SDL.createTextureFromSurface renderingContext loadedSurface
  -- textureInfo   <- SDL.queryTexture newTexture
  -- let [textureWidth, textureHeight] =
        -- map ($ textureInfo) [SDL.textureWidth, SDL.textureHeight]
  SDL.freeSurface loadedSurface
  return $ textureWithClips newTexture
                            (SDL.Rectangle (P $ V2 0 0) (V2 100 100))
                            (SDL.Rectangle (P $ V2 100 0) (V2 100 100))
                            (SDL.Rectangle (P $ V2 0 100) (V2 100 100))
                            (SDL.Rectangle (P $ V2 100 100) (V2 100 100))

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  window <- SDL.createWindow
    "SDL Tutorial"
    SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
  SDL.showWindow window

  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  SDL.rendererDrawColor renderer $= clearColor

  circleSpriteSheet <- loadTextureAndClips "dots.png" renderer

  -- foreground     <- loadTexture "foo.png" renderer

  -- foregroundInfo <- SDL.queryTexture foreground

  -- let foregroundWidth  = SDL.textureWidth foregroundInfo
      -- foregroundHeight = SDL.textureHeight foregroundInfo

  let loop = do
        events <- map SDL.eventPayload <$> SDL.pollEvents
        let quit = SDL.QuitEvent `elem` events
        SDL.clear renderer
        SDL.copy renderer (texture circleSpriteSheet) (Just $ topLeftRect circleSpriteSheet) (Just $ SDL.Rectangle (P $ V2 0 0) (V2 100 100))
        SDL.copy renderer (texture circleSpriteSheet) (Just $ topRightRect circleSpriteSheet) (Just $ SDL.Rectangle (P $ V2 (screenWidth - 100) 0) (V2 100 100))
        SDL.copy renderer (texture circleSpriteSheet) (Just $ bottomLeftRect circleSpriteSheet) (Just $ SDL.Rectangle (P $ V2 0 (screenHeight - 100)) (V2 100 100))
        SDL.copy renderer (texture circleSpriteSheet) (Just $ bottomRightRect circleSpriteSheet) (Just $ SDL.Rectangle (P $ V2 (screenWidth - 100) (screenHeight - 100)) (V2 100 100))
        -- SDL.copy renderer foreground Nothing $ Just $ SDL.Rectangle
          -- (P $ V2 240 190)
          -- (V2 foregroundWidth foregroundHeight)
        SDL.present renderer
        unless quit loop
  loop

  -- SDL.destroyTexture foreground
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit


