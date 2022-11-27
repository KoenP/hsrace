module Network.EventMsg where

import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import FRP.Elerea.Param
import Data.Serialize
import Data.Coerce
import Data.List
import Control.Applicative
import GHC.Generics

import ElereaTools
import IDMap ( ID )
import Vec ( Vec )
import TypeDefs ( PlayerID, SG, Time )
import Basket
import Network.SerializedSize ( SerializedSize )

-- Then we define event messages. These messages must arrive at their
-- destination and be processed in the order they were originally
-- sent. Each event message gets assigned a unique AckID
-- (acknowledgement identifier). The sender keeps re-sending until the
-- receiver sends back an ack with the matching ID.
data EventMsg = EventMsg { _msgAckID        :: AckID
                         , _eventMsgPayload :: EventMsgPayload
                         }
  deriving (Generic, Show, Eq, Ord)
instance Serialize EventMsg
instance SerializedSize EventMsg

newtype AckID = AckID Int deriving (Generic, Show, Eq, Ord)
instance ID AckID
instance Serialize AckID
instance SerializedSize AckID

data EventMsgPayload
  = SpawnPlayerMsg PlayerID Vec -- New player entered the game at position.
  | SpawnBulletMsg Vec Vec      -- A player shot a projectile at pos, in dir.
  | PlayerIDMsg PlayerID        -- Communicate a connecting client their player ID.
  | StartGameMsg                -- When in lobby: start the game.
  | WelcomeToLobby              -- Server sends this to client when
  deriving (Generic, Show, Eq, Ord)
instance Serialize EventMsgPayload

getPlayerIDMsgs :: [EventMsgPayload] -> [PlayerID]
getPlayerIDMsgs msgs = [id | PlayerIDMsg id <- msgs]

getSpawnPlayerMsgs :: [EventMsgPayload] -> [(PlayerID,Vec)]
getSpawnPlayerMsgs msgs = [(id,v) | SpawnPlayerMsg id v <- msgs]

-- | Keep track of a `streamifyEvents` table per client.
streamifyEventsPerClient 
  :: forall id. (Ord id) 
  => Signal (id -> [EventMsg]) -- ^ The messages received per client id
  -> Signal [id] -- ^ Newly connected clients that we should track
  -> Signal [id] -- ^ Clients that have disconnected and can be deleted
  -> SG (Signal (id -> Maybe (Signal [EventMsg])))
streamifyEventsPerClient msgsPerClient newClients disconnectedClients = mdo
  collD <- delay (M.empty) coll
  newSignals :: Signal [(id, Signal [EventMsg])] 
    <- generator $ fmap sequence $ ffmap streamForClient newClients
  let coll = edit <$> newSignals <*> disconnectedClients <*> collD
  return ((M.!?) <$> coll)
  where
    streamForClient :: id -> SG (id, Signal [EventMsg])
    streamForClient id = (id,) <$> streamifyEvents (fmap ($ id) msgsPerClient)

-- | The incoming signal is the list of event messages arriving every frame.
-- The outgoing signal is the same event messages, ordered by ack ID and
-- without missing messages. Some messages may be withheld for a while
-- and arrive at a later frame.
streamifyEvents :: Signal [EventMsg] -> SG (Signal [EventMsg])
streamifyEvents msgs
  = ffmap (\(_,_,x) -> x) $ transfer (AckID 0, M.empty, []) update msgs
  where
    update :: Time -> [EventMsg] -> (AckID, Map AckID EventMsgPayload, [EventMsg])
           -> (AckID, Map AckID EventMsgPayload, [EventMsg])
    update _ msgs (nextID,buffer,_) =
      let
        buffer' = mapInsertMany [(id,p) | EventMsg id p <- msgs] buffer

        -- | Keep grabbing events from the list until we encounter a
        -- discontinuity. Return the next id we expect, the processed
        -- events, and the unprocessed events.
        takeContinuously :: AckID -> [(AckID,p)] -> (AckID,[(AckID,p)],[(AckID,p)])
        takeContinuously next  [] = (next,[],[])
        takeContinuously next ((id,p):rest)
          | id == next = let (final,pre,post) = takeContinuously (AckID $ coerce id+1) rest
                         in  (final,(id,p):pre, post)
          | otherwise  = (next,[],((id,p):rest))

        (nextID', out, _) = takeContinuously nextID (M.toAscList buffer')

      in
        (nextID', mapDeleteMany (map fst out) buffer', map (uncurry EventMsg) out)

-- | Keep track of which event messages need to be sent or re-sent this frame, for
--   a single receiver.
singleReceiverResendTable
  :: Signal [EventMsgPayload] -> Signal [AckID] -> SG (Signal [EventMsg])
singleReceiverResendTable newPayloadsSig acksSig = do
  newPayloadsWithAcksSig <- assignIDs newPayloadsSig
  toResend <- transfer2 M.empty updateTable newPayloadsWithAcksSig acksSig
  return (map (uncurry EventMsg) . M.toList <$> toResend)
  where
    updateTable
      :: Time -> [(AckID,EventMsgPayload)] -> [AckID] -> Map AckID EventMsgPayload 
       -> Map AckID EventMsgPayload 
    updateTable _ newMsgs acks table = edit newMsgs acks table

-- | Keep track of which event messages need to be sent or re-sent this frame, for
--   multiple receivers (spawns and deaths are separately communicated).
multiReceiverResendTable
  :: forall id. Ord id
  => Signal [id]
  -> Signal [id]
  -> Signal (id -> [EventMsgPayload])
  -> Signal (id -> [AckID])
  -> SG (Signal (Map id [EventMsg]))
multiReceiverResendTable newClientsSig clientsToDeleteSig newPayloadsSig acksSig = do
  -- Connect each client with its outbound event messages signal 
  -- and inbound ack ID signal.
  receiverTablesSig :: Signal [(id,Signal [EventMsg])] <- generator $ do
    newClients <- newClientsSig
    let tables :: SG [Signal [EventMsg]]
        tables = sequence [ singleReceiverResendTable myPayloadsSig myAcksSig
                          | id <- newClients
                          , let myAcksSig = fmap ($ id) acksSig
                          , let myPayloadsSig = fmap ($ id) newPayloadsSig
                          ]
    return (fmap (newClients `zip`) tables)
  assocColl receiverTablesSig clientsToDeleteSig
