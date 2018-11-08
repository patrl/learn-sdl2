-- a toy brogue console
--

{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified SDL -- the main sdl2 bindings
-- TODO figure out if juicypixels can replace this, although it's probably slower.
import qualified SDL.Image                     as SDLImg -- necessary to load the brogue font
import qualified SDL.Font                      as TTF
import           SDL.Vect -- We need this for the vector constructors
import           Control.Monad                  ( unless ) -- a handy utility for breaking a loop
import           Foreign.C.Types                ( CInt ) -- since we dealing with a C FFI library, we need to deal with C Integer types
import           Data.Word                      ( Word8 ) -- RGBA colors are stored in this format
import           Control.Lens                   ( (^.) )
import qualified Data.Vector                   as V
import           Control.Concurrent             ( threadDelay )
import qualified Math.Geometry.Grid            as Grid
import           Math.Geometry.Grid.Square

-- screen height and width in tiles.
screenDims :: V2 CInt
screenDims = V2 20 10

-- screenGrid
screenGrid :: RectSquareGrid
screenGrid = rectSquareGrid (fromIntegral $ screenDims ^. _y)
                            (fromIntegral $ screenDims ^. _x)

-- screen grid index to rect
screenGridIndexToRect :: (Int, Int) -> V2 CInt -> SDL.Rectangle CInt
screenGridIndexToRect (xCoord, yCoord) tileDims =
  mkRect (V2 (fromIntegral xCoord) (fromIntegral yCoord) * tileDims) tileDims

-- >>> Grid.indices screenGrid
-- [(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(0,7),(0,8),(0,9),(0,10),(0,11),(0,12),(0,13),(0,14),(0,15),(0,16),(0,17),(0,18),(0,19),(1,0),(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8),(1,9),(1,10),(1,11),(1,12),(1,13),(1,14),(1,15),(1,16),(1,17),(1,18),(1,19),(2,0),(2,1),(2,2),(2,3),(2,4),(2,5),(2,6),(2,7),(2,8),(2,9),(2,10),(2,11),(2,12),(2,13),(2,14),(2,15),(2,16),(2,17),(2,18),(2,19),(3,0),(3,1),(3,2),(3,3),(3,4),(3,5),(3,6),(3,7),(3,8),(3,9),(3,10),(3,11),(3,12),(3,13),(3,14),(3,15),(3,16),(3,17),(3,18),(3,19),(4,0),(4,1),(4,2),(4,3),(4,4),(4,5),(4,6),(4,7),(4,8),(4,9),(4,10),(4,11),(4,12),(4,13),(4,14),(4,15),(4,16),(4,17),(4,18),(4,19),(5,0),(5,1),(5,2),(5,3),(5,4),(5,5),(5,6),(5,7),(5,8),(5,9),(5,10),(5,11),(5,12),(5,13),(5,14),(5,15),(5,16),(5,17),(5,18),(5,19),(6,0),(6,1),(6,2),(6,3),(6,4),(6,5),(6,6),(6,7),(6,8),(6,9),(6,10),(6,11),(6,12),(6,13),(6,14),(6,15),(6,16),(6,17),(6,18),(6,19),(7,0),(7,1),(7,2),(7,3),(7,4),(7,5),(7,6),(7,7),(7,8),(7,9),(7,10),(7,11),(7,12),(7,13),(7,14),(7,15),(7,16),(7,17),(7,18),(7,19),(8,0),(8,1),(8,2),(8,3),(8,4),(8,5),(8,6),(8,7),(8,8),(8,9),(8,10),(8,11),(8,12),(8,13),(8,14),(8,15),(8,16),(8,17),(8,18),(8,19),(9,0),(9,1),(9,2),(9,3),(9,4),(9,5),(9,6),(9,7),(9,8),(9,9),(9,10),(9,11),(9,12),(9,13),(9,14),(9,15),(9,16),(9,17),(9,18),(9,19)]
--
scalingFactor :: Num a => V2 a
scalingFactor = V2 2 2

_font13grid :: V2 CInt
_font13grid = V2 16 16

-- directory where resources reside.
resourceDir :: FilePath
resourceDir = "data/"

-- TODO find out if there's a nice library for interfacing with RGBA data
-- some handy color aliases
black, _clearColor :: V4 Word8
black = V4 0 0 0 0
_clearColor = black

-- takes a texture atlas, together with the rows/columns, and returns an SDL surface
-- together with the dimensions of each tile.
loadSurfaceWithTileDims
  :: FilePath -> V2 CInt -> IO (SDL.Surface, V2 CInt, V2 CInt)
loadSurfaceWithTileDims path gridDims = do
  surface     <- SDLImg.load $ resourceDir ++ path
  surfaceDims <- SDL.surfaceDimensions surface
  return
    ( surface
    , surfaceDims
    , V2 (surfaceDims ^. _x `div` gridDims ^. _x)
         (surfaceDims ^. _y `div` gridDims ^. _y)
    )

-- takes a surface, and loads it as a texture, discarding the surface from memory.
loadTextureFromSurface :: SDL.Surface -> SDL.Renderer -> IO SDL.Texture
loadTextureFromSurface surface renderer = do
  newTexture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface
  return newTexture

-- a helper function from two 2-dimensional vectors to an SDL rectangle.
mkRect :: V2 CInt -> V2 CInt -> SDL.Rectangle CInt
mkRect p = SDL.Rectangle (P p)

-- takes some texture dimensions, and grid dimensions, and returns. 
mkRects :: V2 CInt -> V2 CInt -> IO (V.Vector (SDL.Rectangle CInt))
mkRects textDims gDims = do
  let cDims =
        V2 (textDims ^. _x `div` gDims ^. _x) (textDims ^. _y `div` gDims ^. _y)
  return $ V.fromList
    [ mkRect (V2 px py) cDims
    | px <- [0, cDims ^. _x .. (textDims ^. _x - cDims ^. _x)]
    , py <- [0, cDims ^. _y .. (textDims ^. _y - cDims ^. _y)]
    ]

main :: IO ()
main = do
  SDL.initialize ([SDL.InitVideo] :: [SDL.InitFlag])

  TTF.initialize

  inputFont     <- TTF.load (resourceDir ++ "InputMono-Regular.ttf") 30

  recLineHeight <- TTF.lineSkip inputFont

  putStrLn
    $  "the recommended pixel height of a renderered line of Input mono is"
    ++ show recLineHeight

  -- load font, and get the dimensions of each tile
  (atlasSurface, surfaceDims, tileDims) <- loadSurfaceWithTileDims
    "font-13.png"
    _font13grid
  putStrLn $ "the dimensions of each tile are:" ++ show tileDims -- for debugging purposes

  -- compute the rectangles corresponding to each tile
  rects  <- mkRects surfaceDims _font13grid

  window <- SDL.createWindow
    "SDL Tutorial"
    SDL.defaultWindow { SDL.windowInitialSize = screenDims * (V2 19 37)
                      , SDL.windowHighDPI     = True
                      , SDL.windowMode        = SDL.Windowed
                      } -- load a window -- the dimension is a multiple of tile size

  renderer         <- SDL.createRenderer window (-1) SDL.defaultRenderer -- creates the rendering context

  inputSurface     <- TTF.blended inputFont (V4 maxBound maxBound maxBound 0) "░▒▓"

  inputSurfaceDims <- SDL.surfaceDimensions inputSurface

  inputTexture     <- loadTextureFromSurface inputSurface renderer

  inputRects       <- mkRects inputSurfaceDims (V2 15 1)

  brogueTexture    <- loadTextureFromSurface atlasSurface renderer

  let [_brogueZero, _brogueDownArrow] =
        ([rects V.! 46, rects V.! 25] :: V.Vector (SDL.Rectangle CInt))

  let broguePrintChar charNum gridCoords = SDL.copy
        renderer
        brogueTexture
        (Just $ rects V.! charNum)
        (Just $ screenGridIndexToRect gridCoords tileDims)

  let inputPrintChar charNum gridCoords = SDL.copy
        renderer
        inputTexture
        (Just $ inputRects V.! charNum)
        (Just $ screenGridIndexToRect gridCoords (V2 19 37))

  let
    loop = do
      events <- map SDL.eventPayload <$> SDL.pollEvents
      let quit = SDL.QuitEvent `elem` events
      SDL.clear renderer
      -- inputPrintChar 0 (0,0)
      sequence_ [ inputPrintChar 2 coords | coords <- Grid.indices screenGrid ]
      SDL.present renderer
      unless quit loop
  loop

  SDL.destroyTexture brogueTexture
  SDL.destroyTexture inputTexture
  SDL.destroyRenderer renderer
  SDL.destroyWindow window

  TTF.free inputFont
  TTF.quit
  SDL.quit
