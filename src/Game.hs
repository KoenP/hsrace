module Game where

--------------------------------------------------------------------------------
import Vec
import Angle
import Input
import Track
import Track.CollisionGrid
import Track.Polygon
import SF
import Types
import Util
import Grid
import Editor.Pillar
import Overlay
import Game.Render
import Game.Types

import Graphics.Gloss

import Prelude hiding ((.), id)
import Data.Maybe
import Control.Applicative
--------------------------------------------------------------------------------

k_acceleration, k_drag, k_hookSpeed :: Double
k_acceleration = 0.05 -- 0.2
k_drag         = 0 -- 0.0005

k_dragOffroad  = 0.1
k_hookSpeed    = 4000

hook :: [Pillar] -> (Vec World, Vec World, Bool) ~> Hook
hook pillars = (\(x,y,z) -> (x,selectPillar y, z)) ^>> runMode noHookMode
  where
    selectPillar = selectedPillar pillars

    noHookMode :: Mode (Vec World, Maybe (Vec World), Bool) Hook
    noHookMode = Mode $ arr $ \(startPos, goal, launchHook) ->
      let event = sample launchHook (hookTravellingMode startPos) <*> goal
      in (event, NoHook)

    hookTravellingMode :: Vec World -> Vec World
                       -> Mode (Vec World, Maybe (Vec World), Bool) Hook
    hookTravellingMode startPos goal =
      let time = startPos <-> goal / k_hookSpeed
      in Mode $ proc (_, _, launchHook) -> do
        t <- fmap (/time) clock -< ()
        let pos = lerp startPos goal t
        let releaseEvent = sample (not launchHook) noHookMode
        let attachEvent = sample (t >= 1) (hookAttachedMode goal)
        returnA -< (releaseEvent <|> attachEvent, HookTravelling pos)

    hookAttachedMode :: Vec World -> Mode (Vec World, Maybe (Vec World), Bool) Hook
    hookAttachedMode pos = Mode $ arr $ \(_,_,keepAttached)
      -> (sample (not keepAttached) noHookMode, HookAttached pos)
  
selectedPillar :: [Pillar] -> Vec World -> Maybe (Vec World)
selectedPillar pillars =
  let cellSize = 400
      grid = mkGrid cellSize (repeat () `zip` pillars)
  in fmap fst <$> closestNearby grid 

-- TODO apply a fix to make sure the tether doesn't stretch
spanTether :: Vec World -> Hook -> Vec World -> Maybe (Vec World)
spanTether pos (HookAttached anchor) vel
  | delta <- anchor ^-^ pos
  , theta <- vel `internalAngle` delta
  , theta > (pi/2)
  = Just $ vel `projectOnto` perp delta
spanTether _   _                     _
  = Nothing
  
velocitySF :: ((Vec World, Vec World, Hook) ~> (Vec World, Maybe (Vec World)))
velocitySF = stateful (zeroVec, Nothing) step
  where
    step _ (dPos, acc, hook) (dVel, _) = let vel    = dVel ^+^ acc
                                             tether = spanTether dPos hook vel
                                         in (fromMaybe vel tether, tether)

-- | Main game loop.
game :: Game
game switchTo (GameTrack onRoad pillars trackPic checkpoints) =
  Mode $ proc input -> do
    -- Switch to editor mode.
    changeMode_ <- changeMode switchTo -< input

    -- Orient the player.
    let cursorPos = _input_cursorPos input
      
    -- Position and heading.
    let accelerating = keyDown Accelerating input
    rec
      let
        rotation = vecAngle cursorPos
        thrust | accelerating = rotVec rotation (Vec 0 k_acceleration)
               | otherwise    = zeroVec
      
      dPosition <- delay zeroVec -< position
      dVelocity <- delay zeroVec -< velocity
      dCursorWorldPos <- delay zeroVec -< cursorWorldPos
      hook      <- hook pillars
        -< (dPosition, dCursorWorldPos, keyDown LaunchHook input) -- (input, dPosition, fromPolar k_hookSpeed rotation)
      let dSpeed = norm dVelocity
      
      let dragFactor = if onRoad dPosition then k_drag else k_dragOffroad
      let drag = (- dragFactor * dSpeed ** 2) *^ normalize dVelocity
      (velocity, tether) <- velocitySF -< (dPosition, thrust ^+^ drag, hook)
      position <- cumsum -< velocity

      -- Calculate viewport and cursor world position.
      avgSpeed <- averageRecentHistory 120 -< norm velocity
      let
        (zoomMin, zoomMax) = (0.05, 0.5) -- (0.2, 1)
        zoom               = clamp (zoomMin, zoomMax) $ lerp zoomMax zoomMin (avgSpeed / 27)
        viewPort           = ViewPort position 0 zoom
        cursorWorldPos     = windowCoordsToWorldCoords viewPort cursorPos

    timePassed_ <- timePassed -< ()
    let overlay = fromWindowTop (_input_windowSize input) 70
                $ color white
                $ Graphics.Gloss.scale 0.5 0.5
                $ translatePic (Vec (-20) 0)
                $ text (minutesSecondsCentiseconds timePassed_)
    pic <- render pillars trackPic
      -< RenderData viewPort position rotation accelerating hook
    
    returnA -<
      ( changeMode_
      , Output (pictures [pic, overlay]) Nothing
      )

