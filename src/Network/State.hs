module Network.State where

--------------------------------------------------------------------------------
import Data.Map (Map)
import qualified Data.Map as M
import FRP.Elerea.Param
import Data.Function ( on )

import TypeDefs ( SG )
import Basket
import ElereaTools
--------------------------------------------------------------------------------

-- Keep track of which event messages need to be sent or re-sent this frame.
resendTable :: Ord id
            => Signal [((id,msg),addr)]
            -> Signal [id]
            -> SG (Signal [((id,msg),addr)])
resendTable new acked = mdo
  table' <- delay M.empty table
  let table = do
        as <- acked
        ns <- map (\((x,y),z) -> (x,(y,z))) <$> new
        t  <- table'
        return . M.union (M.fromList ns) . mapDeleteMany as $ t
  return (map (\(x,(y,z)) -> ((x,y),z)) . M.toList <$> table)

mostRecent :: Ord t => t -> a -> Signal [(t,a)] -> SG (Signal (t,a))
mostRecent t0 initial inputs 
  = largestOneYetBy (compare `on` fst) (t0,initial) inputs
