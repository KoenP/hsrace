module Server.Lobby where

--------------------------------------------------------------------------------
import Data.Bool (bool)

import FRP.Elerea.Param
import Server.Communication
import Network.Packet (isConnectPacket, Packet(Packet), containsStartGameMsg)
import Network.Socket (SockAddr)
import Network.EventMsg
import TypeDefs
import ElereaTools (sigScanl)
import Basket (maybeApplicativeSwap)
import Control.Monad (join)
import Control.Monad.Fix (mfix)

--------------------------------------------------------------------------------

lobby :: SGSig [(Packet,SockAddr)]
      -> SG (Signal [(SockAddr, EventMsgPayload)],
             Signal (Maybe [(PlayerID,SockAddr)]))
lobby inboundMsgsGen = do
  inboundMsgs <- inboundMsgsGen 

  -- An empty packet signals a client trying to connect.
  -- It may be a duplicate though, don't just treat this as addresses of new
  -- players!
  let incomingConnections :: Signal [SockAddr]
      incomingConnections = map snd . filter isConnectPacket
          <$> inboundMsgs

  -- Keep track of new players connecting to the lobby.
  (ptable, newPlayerIDs, newPlayerAddrs)
     <- playerAddressTable incomingConnections

  -- Send newly connected players their ID.
  let playerIDMsgs :: Signal [(SockAddr,EventMsgPayload)]
      playerIDMsgs = do
        ids_t   <- newPlayerIDs
        addrs_t <- newPlayerAddrs
        return $ zip addrs_t $ map PlayerIDMsg ids_t

  -- For now, any player can send a "start game" command which
  -- tells the server to leave the lobby and start the game.
  let startGameCommand :: Signal Bool
      startGameCommand = containsStartGameMsg <$> inboundMsgs

      playerTableAtGameStart :: Signal (Maybe [(PlayerID,SockAddr)])
      playerTableAtGameStart = (\b table -> bool Nothing (Just table) b)
        <$> startGameCommand <*> ptable

  return (playerIDMsgs, playerTableAtGameStart)
