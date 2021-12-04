module Editor.Pillar where

--------------------------------------------------------------------------------
import Grid
import Vec
import SF
import Track
import Util
import Editor.Nodes

import Graphics.Gloss

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import qualified Data.Bifunctor as Bifunctor
--------------------------------------------------------------------------------

newtype PillarID = PillarID Int deriving (Eq, Ord, Show)

gridCellSize = pillarRadius * 4

-- pillar :: Pillar -> ((Vec World, Bool) ~> (Pillar, Picture))
-- pillar pos0 = runMode (notDraggingMode pos0)
--   where
--     notDraggingMode pos0 = Mode $ proc (cursor, dragging) -> do
--       let offset = cursor ^-^ pos0
--       let inRange = norm offset <= pillarRadius
--       startedDragging <- risingEdge -< dragging
--       let event = sample (startedDragging && inRange) (draggingMode offset)
--       returnA -< (event, (pos0, renderPillar inRange pos0))
-- 
--     draggingMode offset = Mode $ proc (cursor, dragging) -> do
--       let pos = cursor ^+^ offset
--       let event = sample (not dragging) (notDraggingMode pos)
--       returnA -< (event, (pos, renderPillar True pos))

pillars :: [Pillar] -> (Vec World, Bool, Bool, Bool) ~> ([Pillar], Picture)
pillars pillars0 =
  let
    pillarMap0 = Map.fromList (map PillarID [0..] `zip` pillars0)
    newIds = map PillarID [length pillars0..]
  in proc inputs -> do
    nodesInput_ <- nodesInput newIds -< inputs
    (nodes_, highlighted) <- nodes pillarRadius pillarMap0 -< nodesInput_
    let unhighlightedPillars | Just id <- highlighted = Map.elems (id `Map.delete` nodes_)
                             | otherwise              = Map.elems nodes_
        highlightedPillar | Just id <- highlighted, Just pos <- Map.lookup id nodes_ = [pos]
                          | otherwise                                              = []
    let pic = pictures $ map (renderPillar True) highlightedPillar ++ map (renderPillar False) unhighlightedPillars 
    returnA -< (Map.elems nodes_, pic)

-- pillars :: [Pillar] -> (Vec World, Bool, Bool) ~> ([Pillar], Picture)
-- pillars pillars0 = proc (cursor, dragging, addNew) -> do
--   nextID <- PillarID <$> stateful' 0 (+) -< boolToInt addNew
--   let newPillarEvent
--         = sample addNew ( nextID
--                         , ((cursor, renderPillar False cursor), pillar cursor)
--                         )
--   stoppedDragging <- risingEdge -< not dragging
--   let updateGrid = stoppedDragging || addNew
--   rec
--     dGrid <- delay (mkGrid gridCellSize []) -< grid
--     nearestPillarID <- fmap snd ^<< delay Nothing <<< highlightedPillar
--       -< (cursor, dragging, dGrid)
--     pillarMap <- sparseUpdater
--       ( Map.fromList
--       $ map PillarID [0..]
--         `zip`
--          ([((pos, renderPillar False pos), pillar pos) | pos <- pillars0]))
--       -< ( []
--          , maybeToList newPillarEvent
--          , maybeToList nearestPillarID `zip` [(cursor, dragging)]
--          )
--     let newGrid = mkGrid gridCellSize [ (id,pos)
--                                       | (id,(pos,_)) <- Map.toList pillarMap
--                                       ]
--     grid <- setter (mkGrid gridCellSize []) -< sample updateGrid newGrid
-- 
--   returnA -< Bifunctor.second pictures $ unzip $ Map.elems pillarMap


-- TODO fully duplicated from Editor.hs:highlightedWaypoint
highlightedPillar :: (Vec World, Bool, Grid World PillarID)
                  ~> Maybe (Vec World, PillarID)
highlightedPillar = runMode notDraggingMode
  where
    notDraggingMode = Mode $ proc (cursorPos, tryingToDrag, grid) -> do
      let
        nearestWaypoint = closestNearby grid cursorPos
        dragging = tryingToDrag && isJust nearestWaypoint

      returnA -< ( sample dragging (draggingMode (fromJust nearestWaypoint))
                 , nearestWaypoint
                 )

    draggingMode wp = Mode $ proc (_, tryingToDrag, _) -> do
      returnA -< (sample (not tryingToDrag) notDraggingMode, Just wp)

renderPillar :: Bool -> Vec World -> Picture
renderPillar highlight pos = color white
  $ translatePic pos
  $ if highlight then circleSolidPic pillarRadius else circlePic pillarRadius
