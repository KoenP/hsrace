module Server.Server where

--------------------------------------------------------------------------------
import System.Clock
import Control.Concurrent (threadDelay)
import Control.Applicative
import Data.Either (fromRight)
import Data.Maybe (fromMaybe, fromJust, catMaybes, mapMaybe)
import Data.Functor ((<&>))
import Data.Function
import Data.Map (Map)
import Data.Tuple
import Data.List
import qualified Data.Map as M
import Control.Monad
import Control.Lens
import Network.Socket.ByteString
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Serialize
import Data.IORef
import FRP.Elerea.Param
import Debug.Trace

import Vec
import Angle
import GameLogic
import Constants
import IDMap
import Basket
import TypeDefs
import ElereaTools
import Server.Communication
import Network.State
import Network.EventMsg
import Network.UpdateMsg
import Network.Setup
import Server.Lobby

import qualified Debug.Trace as D
--------------------------------------------------------------------------------

runGameServer :: String -> IO ()
runGameServer port = do
  -- Set up sockets and threads.
  sock     <- setupSocket port
  recvChan <- launchListener sock
  sendChan <- launchSender sock

  -- Initialize clock.
  clock    <- getTime Monotonic >>= newIORef

  -- Set up game signal network.
  (networkMessagesGen, networkMessagesSink) <- external []

  -- Perform game update loop on infinite repeat.
  let
    executeTick :: Tick -> IO ()
    executeTick tick = do
        -- Read network inputs.
        networkMessagesSink =<< readMessages recvChan

        -- Compute delta time.
        let delay = (1 / tickRate) -- seconds
                  * 1000000      -- microseconds
        threadDelay (round delay)
        oldTime <- readIORef clock
        curTime <- getTime Monotonic
        writeIORef clock curTime
        let dt = fromIntegral (toNanoSecs (curTime `diffTimeSpec` oldTime)) / 1000000000

        -- Advance to next frame.
        msgs <- tick dt
    
        -- Send messages to clients.
        sendMessages sendChan msgs

  lobbyProgram networkMessagesGen executeTick

lobbyProgram :: SGSig [(Packet,SockAddr)] -> IO () -> IO ()
lobbyProgram networkMessagesGen executeTick
  = start (lobby networkMessagesGen) >>= runLobby
  where runLobby tick = do
          (output, )executeTick tick
          
gameServer :: SG (Signal [(Packet,SockAddr)])
           -> SG (Signal [(Packet,SockAddr)])
gameServer networkMessagesGen = do
    -- Count frames
    frameNr <- stateful 0 (\_ s -> s+1)
  
    -- Read network messages.
    networkMessages :: Signal [(Packet, SockAddr)] <- networkMessagesGen

    -- Figure out the inbound and outbound acks.
    let
      inboundAcks :: Signal [(SockAddr,AckID)]
      inboundAcks  = map swap . flattenOnFst . map (over _1 _packetAcks)
        <$> networkMessages
    
    let
      outboundAcks :: Signal [(SockAddr,AckID)]
      outboundAcks = map swap . flattenOnFst . map (over _1 (_msgAckID . _packetEvents))
        <$> networkMessages
        -- = concatMap
        --   (\(packet,addr) -> map fst (_packetEvents packet) `zip` repeat addr)
        -- <$> networkMessages

    -- An empty packet signals a client trying to connect.
    -- It may be a duplicate though, don't just treat this as addresses of new
    -- players!
    let incomingConnections :: Signal [SockAddr]
        incomingConnections = map snd . filter isConnectPacket <$> networkMessages
  
    -- Connect new players.
    (ptable, newPlayerIDs, newPlayerAddrs) <- playerAddressTable incomingConnections

    let playerToAddr
          :: Signal (PlayerID -> Maybe SockAddr)
          = fmap (\tbl pid -> M.lookup pid . M.fromList $ tbl) ptable

    let addrToPlayer
          :: Signal (SockAddr -> Maybe PlayerID)
          = fmap (\tbl pid -> M.lookup pid . M.fromList . map swap $ tbl) ptable

    -- Sort messages per player.
    let packetsPerPlayer
          :: Signal [(Packet,PlayerID)]
          = do table <- addrToPlayer
               msgs  <- networkMessages

               -- Filter out packets from unrecognized senders
               -- and translate SockAddrs into PlayerIDs
               return [ (packet, pid)
                      | (packet, addr) <- msgs, let Just pid = table addr
                      ]

    -- Build a lookup table of the inputs received per player, along
    -- with the (client) frame number.
    let
      playerUpdates :: Signal (PlayerID -> [(FrameNr,PlayerUpdate)])
      playerUpdates = do
        lookup :: PlayerID -> [(FrameNr,UpdateMsg)]
          <- multiLookupFromList
              . map swap 
              . updatesAndSenders
              <$> packetsPerPlayer
        let selectInputs = mapMaybe
              $ \(fn,msg) -> case msg of
                               PlayerInputMsg pi -> Just (fn,pi)
                               _                 -> Nothing
        return (selectInputs . lookup)

    let rawEventsPerPlayer :: Signal (PlayerID -> [EventMsg])
        rawEventsPerPlayer 
           =  multiLookupFromList . map swap . eventsAndSenders 
          <$> packetsPerPlayer
    eventsFromPlayers :: Signal (PlayerID -> Maybe (Signal [EventMsg]))
      <- streamifyEventsPerClient rawEventsPerPlayer newPlayerIDs (pure []) 
          -- TODO disconnected clients
          
    
    -- TODO: player deaths
    mostRecentPlayerUpdates 
      :: Signal (Map PlayerID (FrameNr, PlayerUpdate))
      <- trackMostRecentPlayerUpdates newPlayerIDs (pure []) playerUpdates

    -- Compose network messages to send.
    let allAddresses :: Signal [SockAddr] = fmap (map snd) ptable
    -- let worldStateUpdates :: Signal [(ServerToClient,SockAddr)]
    --       = liftA2 zip (fmap (repeat . StC_WorldState) worldState) allAddresses

    -- Updates to send to all players.
    let playerUpdates :: Signal [UpdateMsg]
          = map snd
          . M.toList
          <$> mostRecentPlayerUpdates

    -- We're going to send update messages to everyone.
    -- TODO this is probably quite inefficient
    let addressedUpdates :: Signal [(UpdateMsg,SockAddr)]
        addressedUpdates = liftA2 (liftA2 (,)) playerUpdates allAddresses

    
    -- Send IDs and initialization data to new players.
    let
      -- playerIDMsgs and initMsgs without ack ids.
      playerIDMsgs, initMsgs :: Signal [EventMsgPayload]
      playerIDMsgs = fmap (map PlayerIDMsg) newPlayerIDs
      initMsgs     
        = map (\(pid,PlayerData pos _ _) -> SpawnPlayerMsg pid pos)
        . M.toList
        <$> ps

    let
      addressedInitEvents :: Signal (SockAddr -> [EventMsgPayload])
      addressedInitEvents = do
        addrs  <- newPlayerAddrs
        idMsgs <- playerIDMsgs
        imsgs  <- initMsgs
        let addressedIdMsgs = zip addrs idMsgs
        return $ (\ inputs val ->let
                     alterF v Nothing   = Just [v]
                     alterF v (Just vs) = Just (v : vs)
                     table              = foldl'
                                          (\m (val,msg) -> M.alter (alterF msg) val m)
                                          M.empty
                                          inputs
                  in fromMaybe [] $ M.lookup val table) $ addressedIdMsgs ++ liftA2 (,) addrs imsgs

    let
      newPlayerEvents :: Signal (SockAddr -> [EventMsgPayload])
      newPlayerEvents = do
        ids   <- newPlayerIDs
        addrs <- snd <$$> ptable
        let msgs = [SpawnPlayerMsg id zeroV | id <- ids] 
        return $ (\ val ->let
              alterF v Nothing   = Just [v]
              alterF v (Just vs) = Just (v : vs)
              table              = foldl'
                                   (\m (val,msg) -> M.alter (alterF msg) val m)
                                   M.empty
                                   (liftA2 (,) addrs msgs)
           in fromMaybe [] $ M.lookup val table)

    eventMsgs
      :: Signal [(EventMsg,SockAddr)]
      <- map swap . flattenOnSnd . M.toList <$$> multiReceiverResendTable
           newPlayerAddrs
           (pure []) -- TODO!
           (liftA2 (<>) addressedInitEvents newPlayerEvents)
           ((\ inputs val ->let
                    alterF v Nothing   = Just [v]
                    alterF v (Just vs) = Just (v : vs)
                    table              = foldl'
                                         (\m (val,msg) -> M.alter (alterF msg) val m)
                                         M.empty
                                         inputs
                 in fromMaybe [] $ M.lookup val table) <$> inboundAcks)

    
    -- Build packets.
    let packets = buildPacketsPerAddr
          <$> frameNr <*> outboundAcks <*> addressedUpdates <*> eventMsgs
    
    return packets



  
scoreTable :: Signal [PlayerID] -> SG (Signal (Map PlayerID Int))
scoreTable scores = transfer M.empty update scores
  where
    update _ scores table 
      = foldl' (\t pid -> M.alter alterEntry pid t) table scores
    alterEntry Nothing  = Just 1
    alterEntry (Just n) = Just (n+1)
  
