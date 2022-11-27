module Network.UpdateMsg where

--------------------------------------------------------------------------------

import GHC.Generics ( Generic )
import Data.Serialize ( Serialize )

import SF ( Time, PerSecond(..) )
import Vec
import Angle
-- import TypeDefs ( FrameNr, PlayerID )
import Network.SerializedSize ( SerializedSize )

--------------------------------------------------------------------------------

data WorldUpdateMsg playerId = WorldUpdateMsg
  { _wum_playerUpdates :: [(playerId, PlayerUpdateMsg, Time)]
  , _wum_now           :: Time
  } 
  deriving Generic
instance Serialize id => Serialize (WorldUpdateMsg id)
instance SerializedSize id => SerializedSize (WorldUpdateMsg id)

data PlayerUpdateMsg = PlayerUpdateMsg
  { _pu_pos          :: Vec World
  , _pu_vel          :: PerSecond (Vec World)
  , _pu_rot          :: Angle
  , _pu_accelerating :: Bool
  }
  deriving (Show, Generic) 
instance Serialize PlayerUpdateMsg
instance SerializedSize PlayerUpdateMsg

-- getUpdatePlayerMsgs :: [(FrameNr,UpdateMsg)] -> [(FrameNr,PlayerUpdate)]
-- getUpdatePlayerMsgs msgs = [(fn,upd) | (fn,PlayerUpdateMsg upd) <- msgs]
-- 
-- data PlayerUpdate = PlayerUpdate 
--     { _puPlayerID :: !PlayerID
--     , _puPose     :: !Pose
--     , _puVelocity :: !Vec
--     } deriving (Generic, Show, Eq, Ord)
-- instance Serialize PlayerUpdate
-- instance SerializedSize PlayerUpdate


