{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances #-}

module GameLogic where

import qualified Math.Geometry.Grid            as G
import           Math.Geometry.Grid.Square
import           Apecs
import           Control.Monad

type GameBoard = RectSquareGrid

boardHeight, boardWidth :: Int
boardHeight = 9
boardWidth = 9

board :: GameBoard
board = rectSquareGrid boardHeight boardWidth

newtype Position = Position (Int,Int)
instance Component Position where type Storage Position = Map Position

data Player = Player deriving Show
instance Component Player where type Storage Player = Unique Player

newtype Turn = Turn Int deriving Show
instance Semigroup Turn where (Turn t1) <> (Turn t2) = Turn $ t1 + t2
instance Monoid Turn where mempty = Turn 0
instance Component Turn where type Storage Turn = Global Turn

makeWorld "World" [''Position, ''Player]

type System' a = System World a

playerPos :: (Int, Int)
playerPos = (4, 4)

initialize :: System' ()
initialize = void $ newEntity (Player, Position playerPos)

-- safe function for movement
step :: G.Direction RectSquareGrid -> System' ()
step dir = cmap $ \(Position p) -> case G.neighbour board p dir of
  Nothing     -> Position p
  Just newPos -> Position newPos

-- >>> replicateM_ boardHeight $ putStrLn $ replicate boardWidth '@'
-- @@@@@@@@@
-- @@@@@@@@@
-- @@@@@@@@@
-- @@@@@@@@@
-- @@@@@@@@@
-- @@@@@@@@@
-- @@@@@@@@@
-- @@@@@@@@@
-- @@@@@@@@@
