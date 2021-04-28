module Bin where

draggable :: Double -> Vec World -> ((Vec World, Bool) ~> (Vec World, Bool))
draggable radius pos0 = runMode (notSelectedMode pos0)
  where
    notSelectedMode pos0 = Mode $ proc (cursorWorldPos, selecting) -> do
      let
        offset  = pos0 ^-^ cursorWorldPos
        inRange = norm offset <= radius

      -- not quite correct: draggin will "pick up" points
      event <- sampleOnRisingEdge -< (selecting && inRange, selectedMode offset)
      returnA -< (event, (pos0, inRange))

    selectedMode offset = Mode $ proc (cursorWorldPos, selecting) -> do
      let pos = cursorWorldPos ^+^ offset
      event <- sampleOnRisingEdge -< (not selecting, notSelectedMode pos)
      returnA -< (event, (pos, True))

waypoint :: Waypoint -> (Vec World, WaypointComponent) ~> Waypoint
waypoint (Waypoint anchor0 (cp1_0, cp2_0)) = proc (cursorPos, whichComponent) -> do
      
  let
    moveAnchorEvent = case whichComponent of Anchor        -> Just cursorPos; _ -> Nothing
  anchor <- setter anchor0 -< moveAnchorEvent
  let
    moveCp1Event    = case whichComponent of ControlPoint1 -> Just (cursorPos ^-^ anchor); _ -> Nothing
    moveCp2Event    = case whichComponent of ControlPoint2 -> Just (cursorPos ^-^ anchor); _ -> Nothing
  rec
    cp1_ <- delay cp1_0 -< cp1
    cp2_ <- delay cp2_0 -< cp1
    let cp1rot = cp1 `signedInternalAngle` cp1_
    let cp2rot = cp2 `signedInternalAngle` cp2_

    cp1 <- controlPoint cp1_0 -< (cp2rot <$ moveCp2Event , moveCp1Event)
    cp2 <- controlPoint cp2_0 -< (cp1rot <$ moveCp1Event , moveCp2Event)

  returnA -< Waypoint anchor (cp1,cp2)

testwp :: Input ~> Output
testwp = proc input -> do
  GUI _ cursorWorldPos _ overlay <- gui -< input 

  wp <- waypoint (Waypoint zeroVec (Vec 0 (-100),Vec 0 100)) -< (cursorWorldPos, ControlPoint1)
  let (cp1,cp2) = controlPointsAbsolute wp

  let anchorPic = renderPoint red (anchor wp)
      cp1Pic = renderPoint green cp1
      cp2Pic = renderPoint green cp2

  returnA -< Output (pictures [anchorPic, cp1Pic, cp2Pic]) Nothing


data WaypointComponent = Anchor | ControlPoint1 | ControlPoint2

controlPoint :: Vec World -> (Maybe Angle, Maybe (Vec World)) ~> Vec World
controlPoint offset0 = stateful' offset0 update
  where
    update :: (Maybe Angle, Maybe (Vec World)) -> Vec World -> Vec World
    update (_            , Just newPos) _      = newPos
    update (Just rotation, Nothing    ) oldPos = (-rotation) `rotVec` oldPos
    update _ oldPos                            = oldPos
