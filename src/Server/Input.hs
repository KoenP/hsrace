module Server.Input where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IORef
import Control.Lens

import TypeDefs
import Basket
import Network.EventMsg
import Network.UpdateMsg
import Network.Setup
import Network.Packet
import Server.Communication

  
-- TODO add frame nrs
data ServerInput = ServerInput
  { siNewPlayers    :: [PlayerID]
  , siActivePlayers :: [PlayerID]
  , siPlayerUpdates :: PlayerID -> [(FrameNr, UpdateMsg)]
  , siPlayerEvents  :: PlayerID -> [EventMsgPayload]
  , siFrameNr       :: FrameNr
  }


data PidTable = PidTable { addr2id :: Map SockAddr PlayerID, id2addr :: Map PlayerID SockAddr }

emptyPidTable :: PidTable
emptyPidTable = PidTable Map.empty Map.empty

extendPidTable :: [(SockAddr,PlayerID)] -> PidTable -> PidTable
extendPidTable entries (PidTable a2i i2a)
  = PidTable (mapInsertMany entries a2i) (mapInsertMany (map swap entries) i2a)

constructServerInput :: IORef Int
                     -> IORef PidTable
                     -> IORef (Map (PlayerID,AckID) EventMsgPayload) 
                     -> IORef (Set (SockAddr,AckID))
                     -> IORef FrameNr
                     -> TChan (ByteString, SockAddr)
                     -> IO ServerInput
constructServerInput nextIdRef pidTableRef inboundAckTableRef outboundAcksRef frameNrRef recvChan = do
  -- Update frame number.
  frameNr <- readIORef frameNrRef
  frameNrRef `modifyIORef` (+1)
  
  -- Receive messages
  messages :: [(Packet,SockAddr)] <- readMessages recvChan

  let perAddr :: [(SockAddr, [Packet])]
      perAddr = packetsPerAddr messages

  -- Partition known and unknown clients.
  pidTable <- readIORef pidTableRef
  let msgsFromKnown   :: [((SockAddr,PlayerID),[Packet])]
      msgsFromUnknown :: [(SockAddr,[Packet])]
      (msgsFromKnown, msgsFromUnknown)
        = partitionMaybe
          (\(addr,packets) -> (\pid -> ((addr,pid),packets))
                              <$> Map.lookup addr (addr2id pidTable))
          perAddr


  -- Handle incoming connections.
  -- Filter out only those unknowns that are trying to connect.
  let newAddrs :: [SockAddr]
      newAddrs = map fst $ filter (any packetIsEmpty . snd) msgsFromUnknown

      assignPlayerId :: a -> IO (a,PlayerID)
      assignPlayerId x = do id <- readIORef nextIdRef
                            nextIdRef `writeIORef` (id+1)
                            return (x, PlayerID id)

  newPidTableEntries :: [(SockAddr,PlayerID)] <- mapM assignPlayerId newAddrs
  pidTableRef `modifyIORef` extendPidTable newPidTableEntries

  -- Process messages.
  let acksPerPlayer    :: [((SockAddr,PlayerID), [AckID])]
      updatesPerPlayer :: [((SockAddr,PlayerID), [(FrameNr,UpdateMsg)])]
      eventsPerPlayer  :: [((SockAddr,PlayerID), [EventMsg])]
      (acksPerPlayer, updatesPerPlayer, eventsPerPlayer)
        = unzipAddressedPackets msgsFromKnown

  let byIds :: [((addrs,ids),vals)] -> [(ids,vals)]
      byIds = map (over _1 snd)
      byAddrs :: [((addrs,ids),vals)] -> [(addrs,vals)]
      byAddrs = map (over _1 fst)

  -- Register inbound acks, and remember to ack inbound events by adding outbound acks.
  modifyIORef inboundAckTableRef $ deleteMany (concatSnd (byIds acksPerPlayer))
  modifyIORef outboundAcksRef $ Set.union (Set.fromList (eventAcks (byAddrs eventsPerPlayer)))


  -- Return server input.
  let eventPayloadsPerPlayer = map (over _2 (map _eventMsgPayload)) $ eventsPerPlayer
  activePlayers <- Map.keys . id2addr <$> readIORef pidTableRef
  let serverInput = ServerInput
                    (map snd newPidTableEntries)
                    activePlayers
                    (multiLookupFromListMap (byIds updatesPerPlayer))
                    (multiLookupFromListMap (byIds eventPayloadsPerPlayer))
                    frameNr
  return serverInput
