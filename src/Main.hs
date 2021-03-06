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
import Control.Lens

--------------------------------------------------------------------------------

main :: IO ()
main = do
  -- Read track.
  trackWaypoints <- read <$> readFile "track"
  let trackState = trackStateFromWaypoints trackWaypoints
  
  -- keymap <- constructKeyMap "keybindings.cfg"
  playIO
    FullScreen
    black
    60
    ([], emptyInput, Left (initialGameState trackState))
    (return . render)
    (\e w -> return (registerEvent e w))
    (glossUpdate update)
  where
    mode = InWindow "test" (800,600) (800,600)

update :: Double -> Input -> ProgramState -> IO ProgramState
update dt input (Left gameState   ) = return (updateWorld dt input gameState)
update dt input (Right editorState)
  | Right editorState' <- updateEditor dt input editorState
  = let saveToFile         = view es_saveToFile editorState'
        waypoints          = revRev $ view (es_trackState.ts_waypointsR) editorState'
    in if saveToFile
       then writeFile "track" (show waypoints)
            >> return (Right $ set es_saveToFile False editorState')
       else return (Right editorState')
update dt input (Right editorState) = return (updateEditor dt input editorState)
  -- TODO bad structure

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
