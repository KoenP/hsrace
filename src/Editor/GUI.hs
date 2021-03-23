module Editor.GUI where

--------------------------------------------------------------------------------
import Vec
import SF
import Input
import Util
import Editor.Render

import Graphics.Gloss
--------------------------------------------------------------------------------

-- If you can call it that. Manages the cursor and viewport.

data GUI = GUI { _gui_cursorPos      :: Vec Window
               , _gui_cursorWorldPos :: Vec World
               , _gui_viewPort       :: ViewPort
               , _gui_overlay        :: Picture -> Picture
               }

gui :: Input ~> GUI
gui = proc input -> do
  -- User interface: cursor and viewport.
  let
    adjusting                  = keyDown EditorAdjust input
    mouseMovement              = _input_mouseMovement input
    cursorMovement | adjusting = zeroVec
                   | otherwise = 0.1 *^ mouseMovement
  cursorPos <- cumsum     -< cursorMovement
  viewPort  <- viewPortSF -< input

  let
    renderFun pic  = pictures
      [applyViewPort viewPort pic, plusPicture white cursorPos 7]
    cursorWorldPos = windowCoordsToWorldCoords viewPort cursorPos

  returnA -< GUI cursorPos cursorWorldPos viewPort renderFun
    

viewPortSF :: Input ~> ViewPort
viewPortSF = proc input -> do
  position <- cumsum -< 10 *^ direction input
  returnA -< ViewPort position 0 1

plusPicture :: Color -> Vec w -> Float -> Picture
plusPicture col pos size = color col
  $ translatePic pos
  $ scalePic size
  $ pictures [line [(-1,0),(1,0)] , line [(0,-1),(0,1)]]
