{-# language TypeOperators #-}
{-# language ScopedTypeVariables #-}

module SF where

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

delay :: b -> (a ~> b) -> (a ~> b)
delay b0 (SF sf) = SF $ \dta -> let (b, sf') = sf dta in (b0, delay b sf')

delayMany :: [b] -> (a ~> b) -> (a ~> b)
delayMany bs sf = foldr delay sf bs

sim :: [a] -> (a ~> b) ->  [b]
sim []     _       = []
sim (x:xs) (SF sf) = let (y, sf') = sf (1,x) in y : sim xs sf'

sim' :: Int -> (() ~> b) -> [b]
sim' n sf = sim (replicate n ()) sf 

stateful :: a -> (a -> a) -> (b ~> a)
stateful s0 update = SF $ \_ -> (s0, stateful (update s0) update)

nats = stateful 0 (+1)

negs = fmap negate nats

switch :: (select -> (a ~> b)) -> (a ~> Either select b) -> (a ~> b)
switch select (SF sf) = SF $ \dta -> case sf dta of
  (Left  s, _  ) -> unSF (select s) dta
  (Right b, sf') -> (b, switch select sf')

rSwitch :: (select -> (a ~> Either select b)) -> (a ~> Either select b) -> (a ~> b)
rSwitch select (SF sf) = SF $ \dta -> case sf dta of
  (Left  s, _  ) -> unSF (rSwitch select (select s)) dta
  (Right b, sf') -> (b, rSwitch select sf')

fromInfiniteList :: [b] -> (a ~> b)
fromInfiniteList bs = fmap head $ stateful bs tail

sfCycle :: [b] -> (a ~> b)
sfCycle bs = fmap head $ stateful (cycle bs) tail

col :: Functor f => f (a ~> b) -> (a ~> f b)
col sfs = SF $ \dta -> let fsf = fmap (\(SF sf) -> sf dta) sfs
                      in (fmap fst fsf, col (fmap snd fsf))

rsCycle = rSwitch (const foo) foo
  where foo = sfCycle (map Right [1..5] ++ [Left ()])

-- game, editor :: input ~> Either () picture
-- game = undefined
-- editor = undefined

-- program = switch (const )
