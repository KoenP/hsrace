module InputState where

import Data.Set (Set)
import qualified Data.Set as S
import Graphics.Gloss.Interface.IO.Game ( Key(..)
                                        , KeyState(..)
                                        , SpecialKey(..)
                                        , MouseButton(..)
                                        )
import Control.Lens (makeLenses)
import Data.Maybe (mapMaybe)
import Control.Arrow

import Vec
import KeyMap

type KeyMap = Key -> Maybe Dir
-- data Dir = DirUp | DirDown | DirLeft | DirRight
--          deriving (Eq, Ord, Show, Read) 

data InputState = InputState { _keysDown :: Set Key, _mouseOffset :: Vec }
  deriving (Show)
makeLenses ''InputState

keyboardInput :: KeyMap -> InputState -> Set Dir
keyboardInput keymap = S.fromList . mapMaybe keymap . S.elems . _keysDown

-- moveKeys :: SF (Set Dir, Float) Vec
-- moveKeys = proc (keys, theta) -> do
--   imIntegral zero -< rotVec (-theta) $ ((150*^) . sumV . map dirToVec . S.elems) keys
--   where
--     dirToVec DirLeft  = V2 (-1) 0
--     dirToVec DirRight = V2 1    0
--     dirToVec DirUp    = V2 0    1
--     dirToVec DirDown  = V2 0    (-1)

-- pointerWindowCoords :: Vec -> SF Vec Vec
-- pointerWindowCoords (V2 w h) = clampedIntegral ((V2 (-x) (-y)),(V2 x y)) zero
--   where x = w/2
--         y = h/2
-- 
-- pointerToWorldCoords :: SF (ViewPort, Vec) Vec
-- pointerToWorldCoords = arr (uncurry windowCoordsToWorldCoords)
-- 
-- pointerWorldCoords :: Vec -> SF (Vec, ViewPort) Vec
-- pointerWorldCoords windowDims = proc (input, vp) -> do
--   pw <- pointerWindowCoords windowDims -< input
--   returnA -< windowCoordsToWorldCoords vp pw
