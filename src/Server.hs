module Server where

--------------------------------------------------------------------------------
import SF
import Network.UpdateMsg
import Types

import Prelude hiding ((.))
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List
--------------------------------------------------------------------------------

data ServerInput playerId = ServerInput
  { _si_newPlayers    :: [playerId]
  , _si_playerUpdates :: [(playerId, PlayerUpdateMsg)]
  }
emptyServerInput = ServerInput [] []

serverSF :: Ord id => ServerInput id ~> WorldUpdateMsg id
serverSF = proc (ServerInput newPlayers playerUpdates) -> do
  now <- clock -< ()
  playerUpdates <- playerUpdateTableSF -< (now, playerUpdates)
  returnA -< WorldUpdateMsg playerUpdates now
          
-- | Keeps track of the most recent update for each connected player, with a
--   timestamp indicating when this most recent update arrived.
playerUpdateTableSF :: forall playerId. Ord playerId
                    => (Time, [(playerId, PlayerUpdateMsg)])
                    ~> [(playerId, PlayerUpdateMsg, Time)]
playerUpdateTableSF = (reorderPost . Map.toAscList) ^<< applyMapUpdatesSF <<^ reorderPre
  where 
    reorderPre (now,updates) = [(id,(update,now)) | (id,update) <- updates]
    reorderPost = map (\(x,(y,z)) -> (x,y,z)) 
