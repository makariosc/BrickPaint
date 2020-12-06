module Grid where

import Brick.Types as BT

newtype Board = Board
  { occupied :: [BT.Location]
  }

height, width :: Int
height = 10
width = 10

step :: Board -> Board
step = id

mark :: BT.Location -> Board -> Board
mark v b = Board $ fixLocation v : occupied b

unmark :: BT.Location -> Board -> Board
unmark v b = Board $ filter (/= fixLocation v) (occupied b)

initBoard :: Board
initBoard = Board []

fixLocation :: Location -> Location
fixLocation (Location (x, y)) = Location (x `div` 6, height - 1 - (y `div` 3))
