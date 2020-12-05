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
mark v b = Board $ v : occupied b

initBoard :: Board
initBoard = Board []
