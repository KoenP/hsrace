{-# language TypeOperators #-}
{-# language ScopedTypeVariables #-}
{-# language Arrows #-}

module SF where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow

type Time = Double

data a ~> b = SF { unSF :: (Time,a) -> (b, a ~> b) }

instance Functor ((~>) a) where
  fmap f (SF sf) = SF $ \dta -> let (b, sf') = sf dta in (f b, fmap f sf')

instance Applicative ((~>) c) where
  pure x = SF (\_ -> (x, pure x))
  SF ff <*> SF fa = SF $ \dtc ->
    let (f, sff') = ff dtc
        (a, fa') = fa dtc
    in (f a, sff' <*> fa')

instance Category (~>) where
  id = SF $ \(_,a) -> (a, id)
  SF sg . SF sf = SF $ \(dt,a) ->
    let (b, sf') = sf (dt,a)
        (c, sg') = sg (dt,b)
    in (c, sg' . sf')

instance Arrow (~>) where
  arr f = SF $ \(_,b) -> (f b, arr f)
  first (SF sf) = SF $ \(dt,(b,d)) ->
    let (c,sf') = sf (dt,b)
    in ((c,d), first sf')
  -- TODO other implementations

instance ArrowChoice (~>) where
  -- left :: (b ~> c) -> Either b d ~> Either c d
  left (SF sf) = SF k
    where
      k (dt, Left  b) = let (c, sf') = sf (dt,b) in (Left c, left sf')
      k (dt, Right d) = (Right d, left (SF sf))

instance ArrowLoop (~>) where
  -- loop :: ((b,d) ~> (c,d)) -> (b~>c)
  loop (SF sf) = SF $ \(dt,b) ->
    let ((c,d), sf') = sf (dt,(b,d))
    in (c, loop sf')

-- State
--------
delay :: a -> (a ~> a)
delay a0 = SF $ \(_,a) -> (a0, delay a)

-- delayMany :: [b] -> (a ~> b) -> (a ~> b)
-- delayMany bs sf = foldr delay sf bs

stateful :: a -> (a -> a) -> (b ~> a)
stateful s0 update = SF $ \_ -> (s0, stateful (update s0) update)

risingEdge :: Bool ~> Bool
risingEdge = (&&) <$> fmap not (delay False) <*> id

-- Streams
----------
fromInfiniteList :: [b] -> (a ~> b)
fromInfiniteList bs = fmap head $ stateful bs tail

sfCycle :: [b] -> (a ~> b)
sfCycle bs = fmap head $ stateful (cycle bs) tail

-- Switches
-----------
switch :: (select -> (a ~> b)) -> (a ~> Either select b) -> (a ~> b)
switch select (SF sf) = SF $ \dta -> case sf dta of
  (Left  s, _  ) -> unSF (select s) dta
  (Right b, sf') -> (b, switch select sf')

rSwitch :: (select -> (a ~> Either select b)) -> (a ~> Either select b) -> (a ~> b)
rSwitch select (SF sf) = SF $ \dta -> case sf dta of
  (Left  s, _  ) -> unSF (rSwitch select (select s)) dta
  (Right b, sf') -> (b, rSwitch select sf')

-- Collections
--------------
col :: Functor f => f (a ~> b) -> (a ~> f b)
col sfs = SF $ \dta -> let fsf = fmap (\(SF sf) -> sf dta) sfs
                      in (fmap fst fsf, col (fmap snd fsf))



--------------------------------------------------------------------------------
-- TESTING
--------------------------------------------------------------------------------

nats = stateful 0 (+1)

negs = fmap negate nats

sim :: [a] -> (a ~> b) ->  [b]
sim []     _       = []
sim (x:xs) (SF sf) = let (y, sf') = sf (1,x) in y : sim xs sf'

sim' :: Int -> (() ~> b) -> [b]
sim' n sf = sim (replicate n ()) sf 

counter :: Bool ~> Int
counter = proc reset -> do
  rec output <- returnA -< if reset then 0 else next
      next <- delay 0 -< output+1
  returnA -< output
