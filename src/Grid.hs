module Grid where

--------------------------------------------------------------------------------
import Vec
import Util

import Graphics.Gloss hiding (scale)

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Bifunctor (bimap)
import Data.List
import Data.Function
import Control.Category hiding (id, (.))
--------------------------------------------------------------------------------

data Grid w a = Grid { _gridSize :: !CellSize, _gridMap :: Map (Int,Int) [(Vec w, a)] }
  deriving Show
type CellSize = Double

mkGrid :: CellSize -> [(id, Vec w)] -> Grid w id
mkGrid cellSize ids = Grid cellSize $ multiMapFromList [(scale cellSize pos, (pos,id)) | (id,pos) <- ids]

scale :: CellSize -> Vec w -> (Int,Int)
scale cellSize v = bimap floor floor $ toTup (v ^/ cellSize)

-- TODO: nub is slow
nearby :: Eq a => Double -> Grid w a -> Vec w -> [a]
nearby radius (Grid cellSize grid) pos =
  let
    ((x1,y1), (x2,y2)) = bimap (scale cellSize) (scale cellSize) (pos ^-^ Vec radius radius, pos ^+^ Vec radius radius)
    indices = [(x,y) | x <- [x1..x2], y <- [y1..y2]]
  in
    nub [id | ix <- indices, (_,id) <- multiMapLookup ix grid]


closestNearby :: Grid w a -> Vec w -> Maybe a
closestNearby (Grid cellSize grid) pos =
  let
    (x,y) = scale cellSize pos
    elements = [ (pos <-> itemPos, id)
               | x' <- [x-1,x,x+1], y' <- [y-1,y,y+1]
               , (itemPos, id) <- multiMapLookup (x',y') grid
               ]
  in
    snd <$> safeMinimumBy (compare `on` fst) elements


renderGrid :: Grid w a -> Picture
renderGrid = _gridMap >>> Map.elems >>> concat >>> map (\(pos,_) -> plusPicture white pos 7) >>> pictures
