module Server.Player where

import FRP.Elerea.Param (generator,  Signal )

import Network.UpdateMsg ( PlayerUpdate(..) )
import Network.State ( mostRecent )
import ElereaTools
import TypeDefs (SG, SGSig, PlayerID, FrameNr)
import Basket (AssocColl)
import Vec (V2(..), Vec, Pose(..))

-- holdPlayerInput :: Signal (Maybe PlayerInput) -> SG (Signal PlayerInput)
-- holdPlayerInput = hold (PlayerInput zeroV (V2 0 1) False)

player :: Signal (PlayerID -> [(FrameNr,PlayerUpdate)])
       -> PlayerID
       -> SGSig (FrameNr, PlayerUpdate)
player inputs id = mostRecent (-1) (playerInit id) $ fmap ($ id) inputs

playerInit :: PlayerID -> PlayerUpdate
playerInit id = PlayerUpdate id (Pose 0 (V2 0 1)) 0 

trackMostRecentPlayerUpdates 
    :: AssocColl coll PlayerID
    => Signal [PlayerID]
    -> Signal [PlayerID]
    -> Signal (PlayerID -> [(FrameNr, PlayerUpdate)])
    -> SGSig (coll PlayerID (FrameNr, PlayerUpdate))
trackMostRecentPlayerUpdates spawns deletions inputs = do
    newPlayers_ <- spawnSignals (player inputs) spawns
    assocColl newPlayers_ deletions

{-
How do we initialize new players?
They should receive the following information:
- Their own ID
- The IDs of the other players
- Where the other players are
- (positions and velocities of bullets)
- 

Maybe we should change it up: players can't join a running game,
we have a "lobby" phase instead where players can join.
Once the game is launched from the lobby, we have an init phase,
after which the game starts.

Clients know their own IDs, and the IDs of other clients, from
the lobby phase.
Player positions can be communicated during init phase through
regular update messages, no event messages required.
Init phase could just be on a timer.

-}