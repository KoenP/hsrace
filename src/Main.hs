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
  play FullScreen black 60 ([], emptyInput, Left emptyGameState) track registerEvent (glossUpdate update)

-- update :: Float -> GlossState GameState -> GlossState GameState
-- update dt_float (events, oldInput, world) = ([], newInput, updateGameState dt newInput world)
--   where
--     newInput = constructInput oldInput events
--     dt       = realToFrac dt_float

update :: Double -> Input -> ProgramState -> ProgramState
update dt input (Left gameState) = updateWorld dt input gameState
update dt input (Right editorState) = updateEditor dt input editorState

track :: GlossState ProgramState -> Picture
track (_, _, Left (GameState (Vec x y) _ rot)) =
  let world = rotate (negate $ realToFrac $ _unDegrees $ radToDeg rot)
            $ translate (- realToFrac x) (- realToFrac y)
            $ renderTrack fooTrack
  in pictures [world, playerPic]
track (_, _, Right (EditorState viewPort _ (Vec ptrX ptrY))) = 
  let world = renderTrack fooTrack
      pointer = translate (realToFrac ptrX) (realToFrac ptrY) $ color white $ circle 4
  in pictures [applyViewPort viewPort world, pointer]

fooTrack = constructTrack
          $ TrackDescription
          [ Polar 1400 hardRight
          , Polar 600 hardLeft
          , Polar 800 hardRight
          ]
  where
    hardRight = pi / 2
    hardLeft = -pi / 2

isoscelesTrianglePath :: Float -> Float -> Path
isoscelesTrianglePath base height = [ (-base/2, -height/3)
                                    , ( base/2, -height/3)
                                    , ( 0     ,  height*2/3)
                                    ]
playerPic :: Picture
playerPic = (color red . polygon) (isoscelesTrianglePath 14 23)

applyViewPort :: ViewPort -> Picture -> Picture
applyViewPort (ViewPort (Vec x y) (Radians rot) zoom)
  = scale (realToFrac zoom) (realToFrac zoom)
  . rotate (realToFrac (- rot))
  . translate (realToFrac (-x)) (realToFrac (-y))
