module SF
  ( module SF
  , module Control.Arrow
  , module Control.Category
  ) where

--------------------------------------------------------------------------------
import Vec

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Debug.Trace
--------------------------------------------------------------------------------

type Time = Double

newtype a ~> b = SF { unSF :: (Time,a) -> (b, a ~> b) }

instance Functor ((~>) a) where
  fmap f (SF sf) = SF $ \dta -> let (b, sf') = sf dta in (f b, fmap f sf')

instance Applicative ((~>) c) where
  pure x = SF (const (x, pure x))
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
      k (_ , Right d) = (Right d, left (SF sf))

instance ArrowLoop (~>) where
  -- loop :: ((b,d) ~> (c,d)) -> (b~>c)
  loop (SF sf) = SF $ \(dt,b) ->
    let ((c,d), sf') = sf (dt,(b,d))
    in (c, loop sf')

updateSF :: Time -> i -> (o, (i ~> o)) -> IO (o, (i ~> o))
updateSF dt input (_, SF sf) = return (out, sf')
  where (out, sf') = sf (dt,input)

-- State
--------
delay :: a -> (a ~> a)
delay a0 = SF $ \(_,a) -> (a0, delay a)

-- delayMany :: [b] -> (a ~> b) -> (a ~> b)
-- delayMany bs sf = foldr delay sf bs

stepper :: a -> (a -> a) -> (b ~> a)
stepper s0 update = SF $ const (s0, stepper (update s0) update)

stateful :: s -> (Time -> i -> s -> s) -> (i ~> s)
stateful a0 update = SF sf
  where sf (dt,b) = let a1 = update dt b a0 in (a1, stateful a1 update)

stateWithReset :: a -> (Time -> b -> a -> a) -> ((b, Maybe a) ~> a)
stateWithReset a0 update = SF sf
  where
    sf (dt, (b,Nothing)) = let a1 = update dt b a0 in (a1, stateWithReset a1 update)
    sf (_ , (_,Just a )) = (a, stateWithReset a update)

risingEdge :: Bool ~> Bool
risingEdge = (&&) <$> fmap not (delay False) <*> id

cumsumFrom :: VectorSpace v a => v -> (v ~> v)
cumsumFrom v0 = stateful v0 (const (^+^))

cumsum :: VectorSpace v a => (v ~> v)
cumsum = cumsumFrom zeroVec

integralFrom :: VectorSpace v Time => v -> v ~> v
integralFrom v0 = stateful v0 step
  where step dt v acc = acc ^+^ dt*^v

integral :: VectorSpace v Time => v ~> v
integral = integralFrom zeroVec

clampedIntegralFrom :: (Vec w,Vec w) -> Vec w -> (Vec w ~> Vec w)
clampedIntegralFrom bounds v0 = stateful v0 step
  where step dt v acc = clampVec bounds (acc ^+^ dt*^v)

-- Streams
----------
fromInfiniteList :: [b] -> (a ~> b)
fromInfiniteList bs = head <$> stepper bs tail

sfCycle :: [b] -> (a ~> b)
sfCycle bs = head <$> stepper (cycle bs) tail

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

updateOnJust :: s -> (s -> e -> s) -> (Maybe e ~> s)
updateOnJust b0 f = SF $ \case
  (_, Just a ) -> let b1 = f b0 a in (b1, updateOnJust b1 f)
  (_, Nothing) -> (b0, updateOnJust b0 f)

-- Collections
--------------
col :: Functor f => f (a ~> b) -> (a ~> f b)
col sfs = SF $ \dta -> let fsf = fmap (\(SF sf) -> sf dta) sfs
                      in (fmap fst fsf, col (fmap snd fsf))

-- Events
---------
sample :: Bool -> a -> Maybe a
sample True  a = Just a
sample False _ = Nothing

-- Arrs
-------
arr2 :: (a -> b -> c) -> ((a,b) ~> c)
arr2 = arr . uncurry

--------------------------------------------------------------------------------
-- TESTING
--------------------------------------------------------------------------------

nats = stepper 0 (+1)

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
