module State where

--------------------------------------------------------------------------------
import Vec
import Angle
import Track
import Input

import Control.Lens
--------------------------------------------------------------------------------

data GameState = GameState { _gs_playerPos :: Vec World
                           , _gs_playerVel :: Vec World
                           , _gs_playerRot :: Radians Double
                           }
makeLenses 'GameState

emptyGameState :: GameState
emptyGameState = GameState zeroVec zeroVec 0


data EditorState = EditorState
  { _es_viewPort :: ViewPort
  , _es_curTrack :: TrackZipper
  , _es_pointerPos :: Vec Window
  }
makeLenses 'EditorState

emptyEditorState :: EditorState
emptyEditorState = EditorState (ViewPort zeroVec 0 1) (TrackZipper [] []) zeroVec

type ProgramState = Either GameState EditorState
