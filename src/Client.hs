module Client where

--------------------------------------------------------------------------------
import Types
import Input
import SF
import Vec
import Network.UpdateMsg
--------------------------------------------------------------------------------


clientSF :: (Input, WorldUpdateMsg playerId) ~> (Output, PlayerUpdateMsg)
clientSF = proc (input, WorldUpdateMsg playerUpdates serverTime) -> do

  returnA -< undefined
