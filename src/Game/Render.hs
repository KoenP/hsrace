module Game.Render where

--------------------------------------------------------------------------------
import SF
import Vec
import Angle
import Util
import Track
import Editor.Pillar
import Game.Types

import Graphics.Gloss

import Prelude hiding ((.))
import Data.List
import Debug.Trace
--------------------------------------------------------------------------------

data RenderData = RenderData
  { _rd_viewPort     :: ViewPort
  , _rd_playerPos    :: Vec World
  , _rd_playerRot    :: Angle
  , _rd_accelerating :: Bool
  , _rd_hook         :: Hook
  }

render :: [Vec World] -> Picture -> (RenderData ~> Picture)
render pillars trackPic = proc (RenderData viewPort pos rot accelerating hook) -> do
  -- Render pillars.
  let pillarsPic = pictures
       [ color white $ translatePic pillarPos (circleSolidPic pillarRadius)
       | pillarPos <- pillars]

  -- Render player.
  (playerPic, tracePic) <- renderPlayer      -< (pos, rot, accelerating)
  thrusterPic           <- thrusterAnimation -< (pos, rot, accelerating)
  let hookPic = renderHook pos hook 

  returnA -< pictures
    $ map (applyViewPort viewPort)
      [trackPic, pillarsPic, tracePic, thrusterPic, hookPic, playerPic]

--------------------------------------------------------------------------------
-- Player.

renderPlayer :: (Vec World, Angle, Bool) ~> (Picture, Picture)
renderPlayer = proc (pos, heading, thrusterOn) -> do
  let triangle = map ((^+^ pos) . rotVec heading) (isoscelesTriangle 14 23)
  trace <- recentHistory 10 -< triangle
  let
    -- TODO inefficient
    interpolatedTrace = map polygonPic $ concat
                      $ zipWith (interpolatePolys 4) trace (safeTail trace)
    (r,g,b,_)         = rgbaOfColor red
    alphas            = map (\n -> 1 / fromIntegral n) [1..length interpolatedTrace]
    colors            = [makeColor r g b a | a <- alphas]
    tracePic          = pictures $ zipWith color colors interpolatedTrace
  returnA -< (color red (polygonPic triangle), tracePic)

interpolatePolys :: Int -> [Vec w] -> [Vec w] -> [[Vec w]]
interpolatePolys n vs ws = transpose $ zipWith f vs ws
  where f v w = let samples = [fromIntegral i / fromIntegral n | i <- [0..(n-1)]]
                in map (lerp v w) samples
    
isoscelesTriangle :: Double -> Double -> [Vec w]
isoscelesTriangle base height = [ Vec (-base/2) (-height/3)
                                , Vec ( base/2) (-height/3)
                                , Vec   0       ( height*2/3)
                                ]

--------------------------------------------------------------------------------
-- Thruster.
thrusterAnimation :: (Vec World, Angle, Bool) ~> Picture
thrusterAnimation =
  let
    frames = map (color yellow . translatePic (Vec 0 12) . polygonPic)
             [isoscelesTriangle 5 5, isoscelesTriangle 7 15]
  in
    proc (pos, rotation, thrusterOn) -> do
      pic <- slideShow 0.05 frames -< ()
      returnA -< if thrusterOn
                then translatePic pos (rotatePic (pi + rotation) pic)
                else blank

--------------------------------------------------------------------------------
-- Hooks.

renderHook :: Vec World -> Hook -> Picture
renderHook startPos (HookTravelling endPos)
  = color white
  $ pictures [ linePic [startPos, endPos]
             , translatePic endPos
               $ rotatePic (vecAngle (endPos ^-^ startPos)) hookHead
             ]
renderHook startPos (HookAttached endPos)
  = color white
  $ pictures [linePic [startPos, endPos]
             , translatePic endPos
               $ rotatePic (vecAngle (endPos ^-^ startPos)) hookHead
             ]
renderHook _        _
  = blank

hookHead :: Picture
hookHead = translatePic (Vec 0 7) $ arc (-180) 0 hookRad
  where
    hookRad = 7
