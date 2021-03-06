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

data GameKey
  = Accelerating | Dir Dir | Mode
  | EditorAdjustWidth | EditorPlaceTrack | EditorSave
  deriving (Eq, Ord, Show, Read)

data Input = Input
  { _input_keysDown      :: Set GameKey
  , _input_keysTriggered :: Set GameKey
  , _input_mouseMovement :: Vec Window
  }
makeLenses 'Input

keyDown :: GameKey -> Input -> Bool
keyDown key input = key `Set.member` _input_keysDown input

keyTriggered :: GameKey -> Input -> Bool
keyTriggered key input = key `Set.member` _input_keysTriggered input

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
      = (lookupKey key `insertAll` keysTriggered, keysReleased, mouseMovement)
    step (keysTriggered, keysReleased, mouseMovement) (EventKey key Up _ _)
      = (keysTriggered, lookupKey key `insertAll` keysReleased, mouseMovement)
    step (keysTriggered, keysReleased, mouseMovement) (EventMotion v)
      = (keysTriggered, keysReleased, mouseMovement ^+^ fromTup v)
    step i _ = i

    lookupKey :: Key -> [GameKey]
    lookupKey (MouseButton LeftButton)  = [Accelerating, EditorPlaceTrack]
    lookupKey (MouseButton RightButton) = [EditorAdjustWidth]
    lookupKey (Char 'w')                = [Dir DirUp]
    lookupKey (Char 'a')                = [Dir DirLeft]
    lookupKey (Char 's')                = [Dir DirDown]
    lookupKey (Char 'd')                = [Dir DirRight]
    lookupKey (Char 'm')                = [Mode]
    lookupKey (Char 'z')                = [EditorSave]
    lookupKey _                         = []

    insertAll :: Ord a => [a] -> Set a -> Set a
    insertAll new set = foldl' (flip Set.insert) set new
      
type GlossState w = ([Event], Input, w)

direction :: Input -> Vec w
direction input = sumV $ map toVec $ Set.toList $ _input_keysDown input
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

glossUpdate :: (Double -> Input -> w -> IO w) -> Float -> GlossState w -> IO (GlossState w)
glossUpdate updateWorld dt_float (events, oldInput, w0) = do
  w1 <- updateWorld dt newInput w0
  return ([], newInput, w1)
  where
    newInput = constructInput oldInput events
    dt       = realToFrac dt_float

mouseSensitivity :: Double
mouseSensitivity = 0.0001
