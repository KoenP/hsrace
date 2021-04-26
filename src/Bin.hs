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

waypoint :: Double -> Waypoint -> ((Vec World, Bool) ~> Waypoint)
waypoint radius seg0 = runMode (notSelectedMode seg0)
  where
    notSelectedMode  :: Waypoint -> Mode (Vec World, Bool) Waypoint
    notSelectedMode seg@(Waypoint anchor (p1,p2)) = Mode $ proc (cursorWorldPos, selecting) -> do
      let
        (e1,e2)   = controlPointsAbsolute seg
        offsets   = [v ^-^ cursorWorldPos | v <- [e1, e2, anchor]]
        inRange   = any ((<= radius) . norm) offsets
        nextMode  = snd
          $ minimumBy
            (compare `on` (norm . fst))
            [ (offset, f offset)
            | (offset, f) <- offsets `zip` [ dragControlPointMode False seg
                                          , dragControlPointMode True  (flipWaypointControlPoints seg) 
                                          , dragAnchorMode seg
                                          ]
            ]

      selectingRisingEdge <- risingEdge -< selecting
      event <- sampleOnRisingEdge -< (selectingRisingEdge && inRange, nextMode)
      returnA -< (event, seg)

    dragAnchorMode :: Waypoint -> Vec World -> Mode (Vec World, Bool) Waypoint
    dragAnchorMode seg0 offset = Mode $ proc (cursorWorldPos, selecting) -> do
      let anchor = cursorWorldPos ^+^ offset
      let seg    = seg0 { anchor = anchor }
      event <- sampleOnRisingEdge -< (not selecting, notSelectedMode seg)
      returnA -< (event, seg)

    dragControlPointMode :: Bool -> Waypoint -> Vec World -> Mode (Vec World, Bool) Waypoint
    dragControlPointMode flip (Waypoint anchor (e1init,e2init)) offset = Mode $ proc (cursorWorldPos, selecting) -> do
      let
        e1Absolute = cursorWorldPos ^+^ offset
      e1Absolute_ <- delay (e1init ^+^ anchor) -< e1Absolute
      let
        e1  = e1Absolute  ^-^ anchor
        e1_ = e1Absolute_ ^-^ anchor
        dTheta = e1 `signedInternalAngle` e1_
      theta <- cumsum -< (-dTheta)
      let
        e2  =  theta `rotVec` e2init

      let segment | flip      = Waypoint anchor (e2,e1)
                  | otherwise = Waypoint anchor (e1,e2)
      event <- sampleOnRisingEdge -< (not selecting, notSelectedMode segment)
      returnA -< (event, segment)
