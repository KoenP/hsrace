module Game where

--------------------------------------------------------------------------------
import Angle 
import Vec
import Input
import Track 
import SF
import Types 
import Util
import Grid
import Overlay 
import Game.Render
import Game.Types
import Graphics.Gloss 

import Prelude hiding ((.), id)
import Data.Functor 
import Data.Maybe 
import Control.Applicative
import Control.Monad
--------------------------------------------------------------------------------

-- k_acceleration, k_drag, k_hookSpeed :: Double
-- k_acceleration = 0.05 -- 0.2
-- k_drag         = 0 -- 0.0005
-- 
-- k_dragOffroad :: Double
-- k_dragOffroad  = 0 -- 0.1
-- k_hookSpeed    = 4000

accelerationPS, dragPS, hookSpeedPS :: PerSecond Double
accelerationPS = PerSecond 500
dragPS         = PerSecond 0
hookSpeedPS    = PerSecond 4000

respawnTime :: Time
respawnTime = 3

-- | Computes the behavior of the hook, based on a (static) list of
-- pillars, and (dynamic) start position, currently selected pillar,
-- and a boolean indicating whether the hook should be launched.
hook :: (Vec World, Maybe (Vec World), Bool) ~> Hook
hook = runMode noHookMode
  where
    noHookMode :: Mode (Vec World, Maybe (Vec World), Bool) Hook
    noHookMode = Mode $ arr $ \(startPos, goal, launchHook) ->
      let event = sample launchHook (hookTravellingMode startPos) <*> goal
      in (event, NoHook)

    hookTravellingMode :: Vec World -> Vec World
                       -> Mode (Vec World, Maybe (Vec World), Bool) Hook
    hookTravellingMode startPos goal =
      let time = startPos <-> goal / unPerSecond hookSpeedPS -- k_hookSpeed
      in Mode $ proc (_, _, launchHook) -> do
        t <- fmap (/time) clock -< ()
        let pos = lerp startPos goal t
        let releaseEvent = sample (not launchHook) noHookMode
        let attachEvent = sample (t >= 1) (hookAttachedMode goal)
        returnA -< (releaseEvent <|> attachEvent, HookTravelling pos)

    hookAttachedMode :: Vec World -> Mode (Vec World, Maybe (Vec World), Bool) Hook
    hookAttachedMode pos = Mode $ arr $ \(_,_,keepAttached)
      -> (sample (not keepAttached) noHookMode, HookAttached pos)
  
selectPillar :: [Pillar] -> Vec World -> Maybe (Vec World)
selectPillar pillars =
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
  
-- | Computes velocity based on delayed position, current acceleration
--   and current state of the hookshot. Also outputs the contribution of
--   the hook tether, if relevant.
velocitySF :: ((Vec World, PerSecond (Vec World), Hook)
               ~> (PerSecond (Vec World), Maybe (Vec World)))
velocitySF = stateful (PerSecond zeroVec, Nothing) step
  where
    step dt (dPos, acc, hook) (dVel, _)
      = let vel    = unPerSecond dVel ^+^ frameDelta acc dt
            tether = spanTether dPos hook vel
        in (PerSecond (fromMaybe vel tether), tether)
           
data PlayerInputs = PlayerInputs
  { _pi_accelerating    :: Bool
  , _pi_launchHook      :: Bool
  , _pi_cursorWindowPos :: Vec Window
  , _pi_selectedPillar  :: Maybe (Vec World)
  }
data PlayerOutputs
  = PlayerOutputs { _po_position :: Vec World 
                  , _po_rotation :: Angle
                  , _po_velocity :: PerSecond (Vec World)
                  , _po_alive    :: Bool
                  , _po_hook     :: Hook
                  }
playerAliveMode :: (Vec World -> Bool) -> Vec World -> Mode PlayerInputs PlayerOutputs
playerAliveMode onRoad pos0
  = Mode $ proc input -> do
      rec
        let
          rotation              = vecAngle (_pi_cursorWindowPos input)
          thrust | _pi_accelerating input = rotVec rotation . Vec 0 <$> accelerationPS
                 | otherwise              = PerSecond zeroVec
      
        dPosition <- delay pos0 -< position
        dVelocity <- delay zeroVec -< velocity
        hook      <- hook
          -< (dPosition, _pi_selectedPillar input, _pi_launchHook input) -- (input, dPosition, fromPolar k_hookSpeed rotation)
        let dSpeed = norm dVelocity
        
        let dragFactor = 0 -- if onRoad dPosition then k_drag else k_dragOffroad
        let drag = (- dragFactor * dSpeed ** 2) *^ normalize dVelocity
        (velocity, _tether) <- velocitySF -< (dPosition, thrust ^+^ drag, hook)
        position <- integralFrom pos0 -< velocity

      let deadMode = guard (not (onRoad position)) $> playerDeadMode onRoad position
      returnA -< (deadMode, PlayerOutputs position rotation velocity True hook)
              
playerDeadMode :: (Vec World -> Bool) -> Vec World -> Mode PlayerInputs PlayerOutputs
playerDeadMode onRoad position
  = Mode $ proc _ -> do
      timeLeft :: Time <- integralFrom 3 -< PerSecond (-1)
      let aliveMode = guard (timeLeft <= 0) $> playerAliveMode onRoad (Vec 0 30)
      returnA -< (aliveMode, PlayerOutputs position 0 zeroVec False NoHook)

lapCount :: ((Vec World, Vec World) -> Int) -> (Maybe (Vec World) ~> Int)
lapCount lapBoundaryFn
  = proc pos -> do
      dPos <- delayedDelay -< pos
      realLap <- numcumsum -< fromMaybe 0 (liftA2 (curry lapBoundaryFn) dPos pos)
      stateful' 0 max -< realLap -- return the highest lap so far
    
-- | Main game loop.
game :: Game
game switchTo (GameTrack onRoad pillars trackPic lapBoundaryFn)
  = Mode $ proc input -> do 
      -- Switch to editor mode.
      changeMode_ <- changeMode switchTo -< input
      let cursorWindowPos = _input_cursorPos input
          keyDown' key    = keyDown key input

      rec 
        dCursorWorldPos <- delay zeroVec -< cursorWorldPos
        let selectedPillar = selectPillar pillars dCursorWorldPos
            accelerating = keyDown' Accelerating
            launchHook = keyDown' LaunchHook
        PlayerOutputs position rotation velocity alive hook
          <- runMode (playerAliveMode onRoad (Vec 0 30))
          -< PlayerInputs accelerating launchHook cursorWindowPos selectedPillar
         -- Calculate viewport and cursor world position.
        avgSpeed :: Double <- averageRecentHistory 120 -< norm velocity

        let
          (zoomMin, zoomMax) = (0.3, 0.3) -- (0.2, 1)
          zoom               = clamp (zoomMin, zoomMax) $ lerp zoomMax zoomMin (avgSpeed / 500)
          viewPort           = ViewPort position 0 zoom
          cursorWorldPos     = windowCoordsToWorldCoords viewPort cursorWindowPos

      currentLap <- lapCount lapBoundaryFn -< position <$ guard alive
      returnA -< undefined

  --   newLap <- sampleOnChange 0 -< highestLapSoFar
  --   lapTimes
  --     <- updateOnJust [] (\lts newTime -> lts ++ [newTime])
  --     -< timePassed_ <$ newLap

      timePassed <- clock -< ()
      let 
        clockPic = translatePic (Vec (-20) 0)
                   $ text (minutesSecondsCentiseconds timePassed)
        wsize    = _input_windowSize input
        lapsPic  = fromWindowLeft wsize 10 (text (show currentLap))
        overlay  = fromWindowTop wsize 70 $ color white $ Graphics.Gloss.scale 0.5 0.5
                   $ pictures [clockPic, lapsPic]
                   
      pic <- render pillars trackPic
        -< RenderData
             { _rd_viewPort       = viewPort
             , _rd_playerPos      = position
             , _rd_playerRot      = rotation
             , _rd_accelerating   = accelerating
             , _rd_hook           = hook
             , _rd_selectedPillar = selectedPillar
             , _rd_playerAlive    = alive
             }
      
      returnA -<
        ( changeMode_
        , Output (pictures [pic, overlay]) Nothing
        )

