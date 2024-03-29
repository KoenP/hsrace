module Util (module Util, module Debug.Trace) where

--------------------------------------------------------------------------------
import Vec
import Angle

import Graphics.Gloss

import Control.Monad
import Debug.Trace
import Data.Bifunctor
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
--------------------------------------------------------------------------------

traceResult :: Show a => a -> a
traceResult x = traceShow x x

translatePic :: Vec w -> Picture -> Picture
translatePic (Vec x y) = translate (realToFrac x) (realToFrac y)

rotatePic :: Angle -> Picture -> Picture
rotatePic (Radians theta) = rotate (realToFrac $ 180 * theta / pi)

scalePic :: Real a => a -> Picture -> Picture
scalePic a = let a' = realToFrac a in scale a' a'

linePic :: [Vec w] -> Picture
linePic = line . map toTup

polygonPic :: [Vec w] -> Picture
polygonPic = polygon . map toTup

arcPic :: Angle -> Angle -> Double -> Picture
arcPic a1 a2 rad = let Degrees d1 = toPicAngle a1
                       Degrees d2 = toPicAngle a2
                   in arc d1 d2 (realToFrac rad)

circlePic :: Double -> Picture
circlePic radius = circle (realToFrac radius)

circleSolidPic :: Double -> Picture
circleSolidPic radius = circleSolid (realToFrac radius)

thickLineSegmentPic :: Double -> Vec w -> Vec w -> Picture
thickLineSegmentPic thickness start end
  = polygonPic [ start ^+^ offset
               , start ^-^ offset
               , end ^-^ offset
               , end ^+^ offset
               ]
  where
    offset = (thickness / 2) *^ perp (normalize (end ^-^ start))
    
                        
-- maybeToPic :: (a -> Picture) -> Maybe a -> Picture
-- maybeToPic render (Just a) = render a
-- maybeToPic _      Nothing  = blank
                             
maybeToPic :: Maybe Picture -> Picture
maybeToPic Nothing    = blank
maybeToPic (Just pic) = pic

applyViewPort :: ViewPort -> Picture -> Picture
applyViewPort (ViewPort v rot zoom)
  = scalePic zoom . rotatePic (- rot) . translatePic (neg v)

-- | Repeat the last thing mentioned endlessly.
nag :: [a] -> [a]
nag []      = []
nag [x]    = repeat x
nag (x:xs) = x : nag xs

composeMany :: [a -> a] -> a -> a
composeMany = foldr (.) id

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _     = Nothing

safeTail :: [a] -> [a]
safeTail []     = []
safeTail (_:xs) = xs

safeMinimumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
safeMinimumBy _    [] = Nothing
safeMinimumBy comp l  = Just (minimumBy comp l)

-- | Keep approximately one out of every (n+1) entries in a list.
thinOut :: Int -> [a] -> [a]
thinOut _ []     = []
thinOut n (x:xs) = x : thinOut n (drop n xs)

interleave :: [a] -> [a] -> [a]
interleave (x:xs) (y:ys) = x : y : interleave xs ys
interleave xs     []     = xs
interleave []     ys     = ys
                           
uninterleave :: [a] -> ([a],[a])
uninterleave (x:y:l) = let (xs,ys) = uninterleave l in (x:xs,y:ys)
uninterleave l       = (l,[])

mergeMaybes :: (Maybe a, Maybe b) -> Maybe (Either a b)
mergeMaybes (Just a , _     ) = Just (Left a)
mergeMaybes (Nothing, Just b) = Just (Right b)
mergeMaybes _                 = Nothing

mapDeleteMany :: Ord k => [k] -> Map k a -> Map k a
mapDeleteMany keys map = foldl' (flip Map.delete) map keys

mapInsertMany :: Ord k => [(k,a)] -> Map k a -> Map k a
mapInsertMany kvPairs map = Map.fromList kvPairs `Map.union` map

mapAdjustMany :: Ord k => [(k, a -> a)] -> Map k a -> Map k a
mapAdjustMany adjusters map0 = foldl' (\map (k,f) -> Map.adjust f k map) map0 adjusters 

editMap :: Ord k => [(k,a)] -> [k] -> Map k a -> Map k a
editMap inserts deletes = mapInsertMany inserts . mapDeleteMany deletes

-- | Construct a lookup table with possibly multiple results for each key.
--   The original order is not preserved.
multiMapFromList :: Ord k => [(k,a)] -> Map k [a]
multiMapFromList
  = foldl'
    (\m (k,a) -> Map.alter (cons a) k m)
    Map.empty
  where
    cons a Nothing   = Just [a]
    cons a (Just as) = Just (a:as)

multiMapLookup :: Ord k => k -> Map k [a] -> [a]
multiMapLookup k = join . maybeToList . Map.lookup k

infixr 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

plusPicture :: Color -> Vec w -> Float -> Picture
plusPicture col pos size = color col
  $ translatePic pos
  $ scalePic size
  $ pictures [line [(-1,0),(1,0)] , line [(0,-1),(0,1)]]

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

bimap' :: Bifunctor f => (a -> b) -> f a a -> f b b
bimap' fn = bimap fn fn

toMaybe :: Bool -> a -> Maybe a
toMaybe False _ = Nothing
toMaybe True  a = Just a

minutesSecondsCentiseconds :: RealFrac a => a -> String
minutesSecondsCentiseconds time = concat [show mins, ":", pad (show secs), ".", pad (show csecs)]
  where
    (integralPart, fractionalPart) = properFraction time
    (mins, secs) = integralPart `divMod` 60
    csecs = floor (fractionalPart * 100)
    pad [x] = ['0', x]
    pad xs = xs
