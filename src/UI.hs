module UI where

import Brick
  ( App (..),
    AttrMap,
    AttrName,
    BrickEvent (..),
    EventM,
    Next,
    Widget,
    attrMap,
    attrName,
    continue,
    customMain,
    defaultMain,
    hBox,
    halt,
    neverShowCursor,
    on,
    str,
    vBox,
    withAttr,
    withBorderStyle,
  )
import Brick.Types as BT
import Brick.Widgets.Border as B
import Brick.Widgets.Border.Style as BS
import Brick.Widgets.Center as C
import Brick.Widgets.Core as WC
import Control.Monad (void)
import Graphics.Vty as V
import Grid

data Tick = Tick

type Name = ()

app :: App Board Tick Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const aMap
    }

main :: IO ()
main = do
  let buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v

  initialVty <- buildVty
  void $ customMain initialVty buildVty Nothing app b
  where
    b = initBoard

drawUI :: Board -> [Widget Name]
drawUI g = [C.center $ drawBoard g]

drawBoard :: Board -> Widget Name
drawBoard b =
  withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str "Board") $
      clickable () $ vBox rows
  where
    rows = [hBox $ cellsInRow r | r <- [height - 1, height - 2 .. 0]]
    cellsInRow y = [drawCoord (BT.Location (x, y)) | x <- [0 .. width - 1]]
    drawCoord = drawCell . cellAt
    cellAt c
      | c `elem` occupied b = True
      | otherwise = False

drawCell :: Bool -> Widget Name
drawCell True = withAttr filled cw
drawCell False = withAttr unfilled cw

cw :: Widget Name
cw =
  str
    " ---- \n\
    \|    |\n\
    \ ----"

filled, unfilled :: AttrName
filled = attrName "filled"
unfilled = attrName "unfilled"

aMap :: AttrMap
aMap =
  attrMap
    V.defAttr
    [(filled, V.blue `on` V.blue)]

handleEvent :: Board -> BrickEvent Name Tick -> EventM Name (Next Board)
--handleEvent b (VtyEvent (EvMouseDown col row _ _)) = continue $ mark (Location (col, row)) b
handleEvent b (MouseUp _ (Just BLeft) l) = continue $ mark l b
handleEvent b (MouseUp _ (Just BRight) l) = continue $ unmark l b
handleEvent b (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt b
handleEvent b _ = continue b

--handleEvent b _ = continue $ mark (Location (0, 0)) b