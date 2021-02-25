module Input where

--------------------------------------------------------------------------------
import Vec
import Angle

import Graphics.Gloss.Interface.Pure.Game

import qualified Data.Set as Set
import Data.Set (Set)
import Data.List
import Data.Ix
import Control.Lens
import Debug.Trace
--------------------------------------------------------------------------------

data Dir = DirUp | DirDown | DirLeft | DirRight
  deriving (Eq, Ord, Show, Read)

data GameKey = Accelerating | Dir Dir | Mode
  deriving (Eq, Ord, Show, Read)

data Input = Input
  { _input_keysDown      :: Set GameKey
  , _input_keysTriggered :: Set GameKey
  , _input_mouseMovement :: Vec Window
  }
makeLenses 'Input

emptyInput :: Input
emptyInput = Input Set.empty Set.empty zeroVec

constructInput :: Input -> [Event] -> Input
constructInput (Input down _ _) events
  = Input ((down `Set.union` keysTriggered) Set.\\ keysReleased) keysTriggered mouseMovement
  where
    keysTriggered, keysReleased :: Set GameKey
    mouseMovement :: Vec Window
    (keysTriggered, keysReleased, mouseMovement)
      = foldl' step (Set.empty, Set.empty, zeroVec) events

    step (keysTriggered, keysReleased, mouseMovement) (EventKey key Down _ _)
      | Just gkey <- lookupKey key
      = (gkey `Set.insert` keysTriggered, keysReleased, mouseMovement)
    step (keysTriggered, keysReleased, mouseMovement) (EventKey key Up _ _)
      | Just gkey <- lookupKey key
      = (keysTriggered, gkey `Set.insert` keysReleased, mouseMovement)
    step (keysTriggered, keysReleased, mouseMovement) (EventMotion v)
      = (keysTriggered, keysReleased, mouseMovement ^+^ fromTup v)
    step i _ = i

    lookupKey (MouseButton LeftButton) = Just Accelerating
    lookupKey (Char 'w')               = Just (Dir DirUp)
    lookupKey (Char 'a')               = Just (Dir DirLeft)
    lookupKey (Char 's')               = Just (Dir DirDown)
    lookupKey (Char 'd')               = Just (Dir DirRight)
    lookupKey (Char 'm')               = Just Mode
    lookupKey _                        = Nothing
      
type GlossState w = ([Event], Input, w)

direction :: Set GameKey -> Vec w
direction keys = sumV $ map toVec $ Set.toList keys
  where
    toVec (Dir DirUp)    = upVec
    toVec (Dir DirDown)  = downVec
    toVec (Dir DirLeft)  = leftVec
    toVec (Dir DirRight) = rightVec
    toVec _              = zeroVec

registerEvent :: Event -> GlossState w -> GlossState w
registerEvent ev (events, i, w) = (ev:events, i, w)

-- class GlossInput i where
--   constructInput :: i -> [Event] -> i
--   emptyInput :: i

glossUpdate :: (Double -> Input -> w -> w) -> Float -> GlossState w -> GlossState w
glossUpdate updateWorld dt_float (events, oldInput, w0) = ([], newInput, updateWorld dt newInput w0)
  where
    newInput = constructInput oldInput events
    dt       = realToFrac dt_float

mouseSensitivity :: Double
mouseSensitivity = 0.0001
