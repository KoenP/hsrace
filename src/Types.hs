module Types where

--------------------------------------------------------------------------------
import Input (Input)
import SF
import Track (Track, Layout)

import Graphics.Gloss (Picture)
--------------------------------------------------------------------------------

data FileOutput = FileOutput { _fo_fileName :: String, _fo_content :: String }

data Output = Output Picture (Maybe FileOutput)

type ProgMode = Mode Input Output

type ProgSF = Input ~> Output

type Game = ProgMode -> Layout -> ProgMode
-- type Editor = 
