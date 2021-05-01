module Types where

--------------------------------------------------------------------------------
import Input (Input)
import Vec
import SF
import Track (Track)

import Graphics.Gloss (Picture)
--------------------------------------------------------------------------------

data FileOutput = FileOutput { _fo_fileName :: String, _fo_content :: String }

data Output = Output Picture (Maybe FileOutput)

type ProgMode = Mode Input Output

type ProgSF = Input ~> Output

type Game = ProgMode -> (Vec World -> Bool) -> Picture -> ProgMode
-- type Editor = 
