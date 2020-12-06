module Grid where

import Brick.Types as BT
import qualified Data.DList as DList

newtype Board = Board
  { occupied :: DList.DList BT.Location
  }

height, width :: Int
height = 50
width = 50

step :: Board -> Board
step = id

mark :: BT.Location -> Board -> Board
mark v b = Board $ DList.singleton (fixLocation v) `DList.append` occupied b

unmark :: BT.Location -> Board -> Board
unmark v b = b -- Board $ filter (/= fixLocation v) (occupied b)

initBoard :: Board
initBoard = Board DList.empty -- []

fixLocation :: Location -> Location
fixLocation (Location (x, y)) = Location (x `div` 2, height - 1 - y)
