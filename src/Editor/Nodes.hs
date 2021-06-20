module Editor.Nodes where

--------------------------------------------------------------------------------
import Vec
import SF
import Grid

import Prelude hiding (id, (.))
import Control.Monad
import Data.Coerce
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Data.Function hiding ((.))
import Debug.Trace
--------------------------------------------------------------------------------

data NodesInput id = NodesInput
  { cursor   :: Vec World
  , dragging :: Bool
  , placeNew :: Maybe id
  }

-- pillar    :: Pillar   -> ((Vec World, Bool) ~> (Pillar             , Picture))
-- pillars   :: [Pillar] -> (NodesInput        ~> (Map PillarID Pillar, Picture))
-- 
-- waypoint  :: Waypoint -> ((Vec World, Bool) ~> (Waypoint           , Picture))
-- waypoints :: Cache    -> (NodesInput        ~> (Cache              , Picture))

data State id = State { vectors     :: Map id (Vec World)
                      , highlighted :: Maybe (id, Vec World)
                      -- | The id of the currently highlighted node (if any), as well as the distance to the cursor when we started dragging.
                      }

nodes :: forall id. Ord id => Double -> Map id (Vec World) -> (NodesInput id ~> (Map id (Vec World), Maybe id))
nodes radius init = ((,) <$> vectors <*> fmap fst . highlighted) <$> stateful' (State init Nothing) step
  where
    step :: NodesInput id -> State id -> State id

    -- We are neither dragging nor inserting. Only update the
    -- "currently highlighted node".
    step (NodesInput cursor False Nothing) (State vecs _)
      = State vecs (highlight cursor vecs)

    -- We are dragging a node: the `dragging` input is True and a node
    -- is currently highlighted.
    step (NodesInput cursor True Nothing) (State vecs (Just (id, offset)))
      = State (Map.insert id (cursor ^+^ offset) vecs) (Just (id, offset))
-- offset = pos - cursor
-- pos = cursor + offset

    -- Place a new node.
    step (NodesInput cursor _ (Just newId)) (State vecs _)
      = State (Map.insert newId cursor vecs) (Just (newId, zeroVec))

    -- Remaining case: we're trying to drag, but no node is highlighted.
    -- In this case, we inhibit highlighting.
    step _ state = state

    highlight :: Vec World -> Map id (Vec World) -> Maybe (id, Vec World)
    highlight cursor vecMap
      | Map.null vecMap = Nothing
      | otherwise = let (id,pos,dst) = minimumBy (compare `on` (\(_,_,x) -> x))
                                       [(id, pos, pos <-> cursor) | (id,pos) <- Map.toList vecMap ]
                    in guard (dst <= radius) >> return (id, pos ^-^ cursor)

nodesInput :: [id] -> ((Vec World, Bool, Bool) ~> NodesInput id)
nodesInput ids = proc (cursor, draggingKeyDown, placeNewKeyDown) -> do
  placeNew <- risingEdge -< placeNewKeyDown
  newIdEvent <- snack ids -< placeNew
  returnA -< NodesInput cursor draggingKeyDown newIdEvent

      
-- nodes :: Coercible Int id
--       => (Vec World -> Map id nodeObj -> nodeObj)
--       -> (nodeObj -> ((Vec World, Bool) ~> nodeObj))
--       -> (nodeObj -> [Vec World])
--       -> (nodeObj -> Picture)
--       -> Map id nodeObj
--       -> (NodesInput ~> (Map id nodeObj, Picture))
-- nodes initialize sf nodeObjPositions renderNodeObj cache0 =
--   let
--     nextID0 = case Map.lookupMax cache0 of
--                 Nothing      -> 0
--                 Just (id, _) -> coerce id
--   in 
--     proc (NodesInput cursor dragging placeNew) -> do
--       -- Update next ID.
--       nextID <- coerce <$> stateful' nextID0 (+) -< boolToInt placeNew
-- 
--       -- Determine when to update the grid.
--       stoppedDragging <- risingEdge -< not dragging
--       let updateGrid = stoppedDragging || placeNew
-- 
--       rec
--         dCache <- delay cache0 -< cache
-- 
--         -- Adding new nodes.
--         let
--           newNodeObj      = initialize cursor dCache
--           newNodeObjEvent = sample addNew (nextID, newNodeObj)
-- 
--         dGrid <- delay (mkGrid
-- 
--         let cache = undefined
-- 
--       returnA -< undefined

-- nodes waypoint nodePositions = waypoints
