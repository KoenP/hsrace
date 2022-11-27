module Types where

--------------------------------------------------------------------------------
import Input (Input)
import SF
import Track (GameTrack)

import Graphics.Gloss (Picture)
import Network.UpdateMsg
--------------------------------------------------------------------------------

data FileOutput = FileOutput { _fo_fileName :: String, _fo_content :: String }
  deriving Show

data Output = Output Picture (Maybe FileOutput)
            
type PlayerID = Int

type ProgMode = Mode Input Output

type ProgSF = Input ~> Output

type Game = ProgMode -> GameTrack -> ProgMode
