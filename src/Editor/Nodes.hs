module Editor.Nodes where

--------------------------------------------------------------------------------
import Vec
import SF
import Grid

import Data.Coerce
--------------------------------------------------------------------------------

data NodesInput = NodesInput
  { cursor   :: Vec World
  , dragging :: Bool
  , placeNew :: Bool
  }

pillar    :: Pillar   -> ((Vec World, Bool) ~> (Pillar             , Picture))
pillars   :: [Pillar] -> (NodesInput        ~> (Map PillarID Pillar, Picture))

waypoint  :: Waypoint -> ((Vec World, Bool) ~> (Waypoint           , Picture))
waypoints :: Cache    -> (NodesInput        ~> (Cache              , Picture))

      
nodes :: Coercible Int id
      => (Vec World -> Map id nodeObj -> nodeObj)
      -> (nodeObj -> ((Vec World, Bool) ~> nodeObj))
      -> (nodeObj -> [Vec World])
      -> (nodeObj -> Picture)
      -> Map id nodeObj
      -> (NodesInput ~> (Map id nodeObj, Picture))
nodes initialize sf nodeObjPositions renderNodeObj cache0 =
  let
    nextID0 = case Map.lookupMax cache0 of
                Nothing      -> 0
                Just (id, _) -> coerce id
  in 
    proc (NodesInput cursor dragging placeNew) -> do
      -- Update next ID.
      nextID <- coerce <$> stateful' nextID0 (+) -< boolToInt placeNew

      -- Determine when to update the grid.
      stoppedDragging <- risingEdge -< not dragging
      let updateGrid = stoppedDragging || placeNew

      rec
        dCache <- delay cache0 -< cache

        -- Adding new nodes.
        let
          newNodeObj      = initialize cursor dCache
          newNodeObjEvent = sample addNew (nextID, newNodeObj)

        dGrid <- delay (mkGrid

        let cache = undefined

      returnA -< undefined

-- nodes waypoint nodePositions = waypoints
