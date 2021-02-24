module Main where

--------------------------------------------------------------------------------

import Track
import RenderTrack
import Vec
import Angle
import Input
import Game
import Editor
import State

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Data.List
import Data.Coerce
import Data.Array

--------------------------------------------------------------------------------

main :: IO ()
main = do
  -- keymap <- constructKeyMap "keybindings.cfg"
  play FullScreen black 60 ([], emptyInput, Left emptyGameState) render registerEvent (glossUpdate update)

update :: Double -> Input -> ProgramState -> ProgramState
update dt input (Left gameState   ) = updateWorld dt input gameState
update dt input (Right editorState) = updateEditor dt input editorState

render :: GlossState ProgramState -> Picture
render (_, _, Left  gs) = renderGameState gs
render (_, _, Right es) = renderEditorState es
