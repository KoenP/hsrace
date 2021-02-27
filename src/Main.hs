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
  where
    mode = InWindow "test" (800,600) (800,600)

update :: Double -> Input -> ProgramState -> ProgramState
update dt input (Left gameState   ) = updateWorld dt input gameState
update dt input (Right editorState) = updateEditor dt input editorState

render :: GlossState ProgramState -> Picture
-- render _ = let track = zipWith color (cycle [dim aquamarine,dim blue]) $ cornersToPics $ trackCorners testje -- , (Vec 100 650, 100)]
--                waypoints = map (\(Vec x y,_) -> color white $ translate (realToFrac x) (realToFrac y) $ circle 10) testje
--            in pictures (track ++ waypoints)
render (_, _, Left  gs) = renderGameState gs
render (_, _, Right es) = renderEditorState es

-- cornersToPics :: ([Vec World], [Vec World]) -> [Picture]
-- cornersToPics (ls, rs) -- = map (\(Vec x y) -> color white $ translate (realToFrac x) (realToFrac y) $ circle 10) (ls ++ rs)
--   = zipWith4 f ls (tail ls) (tail rs) rs
--   where
--     f l1 l2 r2 r1 = polygon $ map convert [l1,l2,r2,r1]
--     convert (Vec x y) = (realToFrac x, realToFrac y)
