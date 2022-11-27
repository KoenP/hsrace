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
import Data.Functor
import System.Random
import Debug.Trace

import Control.Monad
--------------------------------------------------------------------------------

data RenderData = RenderData
  { _rd_viewPort        :: ViewPort
  , _rd_playerPoss      :: [Vec World]
  , _rd_playerRots      :: [Angle]
  , _rd_acceleratings   :: [Bool]
  , _rd_hook            :: Hook
  , _rd_selectedPillar  :: Maybe (Vec World)
  , _rd_playerAlive     :: Bool
  }
                
stars :: Picture
stars = color white $ pictures $ take nStars
        $ map (\pos -> translatePic pos (circle 1)) positions
  where
    positions = uncurry (zipWith Vec) (uninterleave noise)
    noise = randomRs (-2000,2000) $ mkStdGen 5846735684
    nStars = 500
 
pillarHighlighter :: Picture   
pillarHighlighter = color white $ pictures
                    [arcPic (fmap (2*i*) arcAngle) (fmap ((2*i+1)*) arcAngle) radius
                    | i <- [0..n-1]
                    ]
  where
    arcAngle = Radians (pi / n)
    n = 6
    radius = pillarRadius + 16

render :: [Vec World] -> Picture -> (RenderData ~> Picture)
render pillars trackPic =
  proc (RenderData
        viewPort
        [pos0,pos1]
        [rot0,rot1]
        [accelerating0,accelerating1]
        hook
        selectedPillar
        playerAlive) -> do

  -- Render pillars.
  let pillarsPic = pictures
       [ color white $ translatePic pillarPos (circleSolidPic pillarRadius)
       | pillarPos <- pillars]
  dt <- timeDelta -< ()
  pillarHighlightRot <- rotator (Radians 0) -< Radians (0.5 * dt)
  let pillarHighlightPic
        = maybeToPic (selectedPillar <&> \pos ->
                        translatePic pos (rotatePic pillarHighlightRot pillarHighlighter))

  -- Render players.
  player0Pic <- renderPlayer      -< (pos0, rot0, accelerating0, playerAlive)
  player1Pic <- renderPlayer      -< (pos1, rot1, accelerating1, playerAlive)
  let hookPic = renderHook pos0 hook 

  tracePic    <- renderPlayerTrace -< pos0 <$ guard playerAlive

  returnA -< pictures
    $ stars : map (applyViewPort viewPort)
      [trackPic, pillarsPic, pillarHighlightPic, tracePic, hookPic, player0Pic, player1Pic]

--------------------------------------------------------------------------------
-- Player.

renderPlayer :: (Vec World, Angle, Bool, Bool) ~> Picture
renderPlayer = runMode renderAlive
  where 
    renderAlive :: Mode (Vec World, Angle, Bool, Bool) Picture
    renderAlive = Mode $ proc (pos, rot, thrusterOn, alive) -> do
                    let triangle = map ((^+^ pos) . rotVec rot) (isoscelesTriangle 28 46)
                    let playerPic = color red (polygonPic triangle)
                    thrusterPic <- thrusterAnimation -< (pos, rot, thrusterOn)
                    let switchMode = guard (not alive) $> renderDead
                    returnA -< (switchMode, pictures [thrusterPic, playerPic])

    renderDead :: Mode (Vec World, Angle, Bool, Bool) Picture
    renderDead = Mode $ proc _ -> do 
                   timePassed <- clock -< ()
                   let switchMode = guard (timePassed > 3) $> renderAlive
                   returnA -< (switchMode, blank)
                   
                            
playerTriangle :: Vec World -> Angle -> Picture
playerTriangle pos heading
    = polygonPic
    $ map ((^+^ pos) . rotVec heading) (isoscelesTriangle 28 46)

traceDuration :: Double
traceDuration = 0.8 -- seconds
                
renderPlayerTrace :: Maybe (Vec World) ~> Picture
renderPlayerTrace = proc player -> do
  now <- clock -< ()                      
  pastPositions <- recentHistoryByTime traceDuration -< player
  returnA -< positionsToPicture (relativePast now pastPositions)
  where 
    positionsToPicture :: [(Maybe (Vec World),Time)] -> Picture
    positionsToPicture [] = blank
    positionsToPicture positions = pictures
      [ color (segmentColor t) (thickLineSegmentPic (maxThickness * (traceDuration - t) / traceDuration) p1 p2)
      | ((Just p2,t),(Just p1,_)) <- zip positions (tail positions)]
                         
    maxThickness :: Double
    maxThickness = 20

    segmentColor :: Time -> Color
    segmentColor t = makeColor 1 0 0 (realToFrac $ 0.5 - (t / (2*traceDuration)))

    relativePast :: Time -> [(a,Time)] -> [(a,Time)]
    relativePast now xts = [(x, now - t) | (x, t) <- xts]


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
    frames = map (color yellow . translatePic (Vec 0 24) . polygonPic)
             [isoscelesTriangle 10 10, isoscelesTriangle 14 30]
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
