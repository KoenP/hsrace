module Server.Shell where

import FRP.Elerea.Param
import System.Clock
import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Lens
import Data.Serialize
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (nub, partition)
import Data.Maybe (isJust)
import Data.Bool
import Data.Tuple

import TypeDefs
import Vec
import Angle
import Network.EventMsg
import Network.UpdateMsg
import Network.Setup
import Network.Packet
import Server.Communication
import Server.Input
import Basket

data ServerOutput = ServerOutput
  { soPlayerUpdates :: [(PlayerID, [UpdateMsg])]
  , soPlayerEvents  :: [(PlayerID, [EventMsg])]
  , soContinueWith  :: Maybe Program
  }


allEvents :: ServerInput -> [EventMsgPayload]
allEvents si = concatMap (siPlayerEvents si) (siActivePlayers si)

emptyServerInput :: ServerInput
emptyServerInput = ServerInput [] [] (const []) (const []) 0

tickRate :: Double
tickRate = 60

type Program = SGSig ServerInput -> SGSig ServerOutput
type Tick = SGSig ServerInput -> Time -> IO ServerOutput

programToTick :: Program -> Tick
programToTick prog = _

run :: IO ()
run = do
  executeTick <- setupProgram "5636"
  undefined
  
lobby :: Program
lobby serverInputGen = do
  serverInput <- serverInputGen 

  -- Send newly connected players their ID.
  -- let playerIDMsgs :: Signal [(SockAddr,EventMsgPayload)]
  --     playerIDMsgs = do
  --       ids_t   <- newPlayerIDs
  --       addrs_t <- newPlayerAddrs
  --       return $ zip addrs_t $ map PlayerIDMsg ids_t

  -- For now, any player can send a "start game" command which
  -- tells the server to leave the lobby and start the game.
  let startGameCommand :: Signal Bool
      startGameCommand = elem StartGameMsg . allEvents <$> serverInput

  let continue = do
        startCmd_   <- startGameCommand
        curPlayers_ <- siActivePlayers <$> serverInput
        pure $ if startCmd_ then Just (sync curPlayers_) else Nothing

  return (ServerOutput [] [] <$> continue)

{- | Initialize the game state, and wait for all players to ack.
-}
sync :: [PlayerID] -> Program
sync ids serverInputGen = do
  -- let initMsgs :: [EventMsgPayload]
  --     initMsgs = initState 50 ids
  undefined

initState :: Double -> [id] -> [(id, Vec)]
initState radius ids = zip ids $ iterate (rotVec alpha) (V2 radius 0)
  where n     = length ids
        alpha = Radians $ 2*pi / fromIntegral n

setupProgram :: String -> IO (Tick -> IO ())
setupProgram port = do
  -- Set up sockets and threads.
  sock     <- setupSocket port
  recvChan <- launchListener sock
  sendChan <- launchSender sock

  -- Initialize clock and frame counter.
  clockRef    <- getTime Monotonic >>= newIORef
  frameNrRef  <- newIORef 0

  -- Set up network state.
  nextPlayerIdRef     <- newIORef 0
  pidTableRef         <- newIORef emptyPidTable
  inboundAckTableRef  <- newIORef Map.empty
  outboundAckTableRef <- newIORef Set.empty

  -- Set up game signal network.
  (serverInputGen, serverInputSink) <- external emptyServerInput

  -- Perform game update loop on infinite repeat.
  return $ \tick -> do
    -- Read network inputs.
    serverInput
      <- constructServerInput nextPlayerIdRef pidTableRef inboundAckTableRef outboundAckTableRef frameNrRef recvChan
    serverInputSink serverInput
  
    -- Compute delta time.
    let delay = (1 / tickRate) -- seconds
              * 1000000      -- microseconds
    threadDelay (round delay)
    oldTime <- readIORef clockRef
    curTime <- getTime Monotonic
    clockRef `writeIORef` curTime
    let dt = fromIntegral (toNanoSecs (curTime `diffTimeSpec` oldTime)) / 1000000000

    -- Advance to next frame.
    ServerOutput updates events cont  <- tick serverInputGen dt
   
    -- Send messages to clients.
    acksToSend <- Set.elems <$> readIORef outboundAckTableRef
    idLookup :: (PlayerID -> SockAddr) <- (Map.!) . id2addr <$> readIORef pidTableRef
    let convert :: [(PlayerID,[a])] -> [(SockAddr,a)]
        convert l = [(addr,a) | (id,as) <- l, let addr = idLookup id, a <- as]
    let packets
          = map swap $ buildPacketsPerAddr (siFrameNr serverInput) acksToSend (convert updates) (convert events)
    sendMessages sendChan packets
    
