module Types where

--------------------------------------------------------------------------------
import Input (Input)
import Vec
import SF
import Track (GameTrack)

import Graphics.Gloss (Picture)
--------------------------------------------------------------------------------

data FileOutput = FileOutput { _fo_fileName :: String, _fo_content :: String }
  deriving Show

data Output = Output Picture (Maybe FileOutput)

type ProgMode = Mode Input Output

type ProgSF = Input ~> Output

type Game = ProgMode -> GameTrack -> ProgMode
-- type Editor = 
