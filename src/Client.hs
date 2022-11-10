module Client where

--------------------------------------------------------------------------------
import SF
import Vec
--------------------------------------------------------------------------------

data NetworkInput id = NetworkInput { _ni_playerPositions :: [(id, Vec World)] }


-- client :: (NetworkInput, )
