module Editor.Overlay where

--------------------------------------------------------------------------------
import Vec
import Input

import Control.Lens
--------------------------------------------------------------------------------

-- | Logic for the overlay elements of the editor, such as mouse, viewport, etc.

data OverlayState
  = OverlayState
  { _os_pointerPos :: Vec Window
  , _os_viewPort   :: ViewPort
  , _os_saveToFile :: Bool
  }

initialOverlayState :: OverlayState
initialOverlayState = OverlayState zeroVec (ViewPort zeroVec 0 1) False

-- | Update the state of the editor overlay.
updateOverlay :: Input -> OverlayState -> OverlayState
updateOverlay input (OverlayState pointerPos0 viewPort0 _) =
  let
    pointerPos1 | lockPointer input = pointerPos0
                | otherwise         = updateCursor (view input_mouseMovement input) pointerPos0
    viewPort1   = updateEditorViewPort input viewPort0
    saveToFile1 = keyTriggered EditorSave input
  in
    OverlayState pointerPos1 viewPort1 saveToFile1
  
updateEditorViewPort :: Input -> ViewPort -> ViewPort
updateEditorViewPort input = updateViewPort dv drot dzoom 
  where
    dv    = 10 *^ direction input
    drot  = 0
    dzoom = 0

lockPointer :: Input -> Bool
lockPointer = keyDown EditorAdjust

updateCursor :: Vec Window -> Vec Window -> Vec Window    
updateCursor mouseMovement cursorPos0 = cursorPos0 ^+^ (0.1 *^ ignoreCoordinateSystem mouseMovement)
