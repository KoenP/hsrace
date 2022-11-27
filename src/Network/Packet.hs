module Network.Packet where

--------------------------------------------------------------------------------
import Control.Lens
    ( none,
      over,
      set,
      makeLenses,
      ASetter,
      Field1(_1),
      Field2(_2),
      Field3(_3) )
import Control.Applicative
import Data.List ( foldl', sort, groupBy, sortOn )
import Data.Maybe ( fromMaybe )
import Data.Serialize ( Serialize, decode, encode )
import GHC.Generics ( Generic )
import qualified Data.Map as M
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Control.Monad.State (State)
import qualified Control.Monad.State as State
import Test.QuickCheck
import Data.Tuple (swap)
import Data.Function (on)

import Network.EventMsg (AckID(..), EventMsg(..), EventMsgPayload(..))
import Network.UpdateMsg (PlayerUpdate(..), UpdateMsg(..) )
import Network.SerializedSize ( SerializedSize(..) )
import TypeDefs ( FrameNr )
import Constants (msgLength)
import Basket ( safeMaximum, concatSnd )

--------------------------------------------------------------------------------

-- | Packets are the data structure that is serialized
-- and sent over the network, after which it is deserialized by the receiver.
-- A packet is essentially a categorized list of messages, all meant for the
-- same recipient. We try to fit as many messages for the same
-- recipient into as few packets as possible (well, not really as
-- possible, but close enough).
data Packet = Packet 
  { _packetFrameNr :: FrameNr
  , _packetAcks    :: [AckID]
  , _packetUpdates :: [UpdateMsg]
  , _packetEvents  :: [EventMsg]
  }
  deriving (Generic, Show)
makeLenses ''Packet

instance Serialize Packet

emptyPacket :: FrameNr -> Packet
emptyPacket frameNr = Packet frameNr [] [] []

packetIsEmpty :: Packet -> Bool
packetIsEmpty (Packet _ [] [] []) = True
packetIsEmpty _                   = False

isConnectPacket :: (Packet, addr) -> Bool
isConnectPacket = packetIsEmpty . fst

-- | Given a bunch of messages to send, each tagged with their
-- recipient, construct a list of packets, each tagged with their
-- recipient.
-- TODO: think about a nicer way to do this
buildPacketsPerAddr
  :: forall addr. Ord addr
  => FrameNr
  -> [(addr, AckID     )]
  -> [(addr, UpdateMsg )]
  -> [(addr, EventMsg  )]
  -> [(addr, Packet    )]
buildPacketsPerAddr frameNr acks updates events =
  let
    alterF lens msg (Just ls)  = Just $ over lens (msg:) ls
    alterF lens msg Nothing    = Just $ over lens (msg:) ([],[],[])
    consAt lens map (addr,msg) = M.alter (alterF lens msg) addr map
    process lens msgs map      = foldl' (consAt lens) map msgs

    msgsPerAddr :: [(addr,([AckID],[UpdateMsg],[EventMsg]))]
    msgsPerAddr = M.toList . process _3 events . process _2 updates . process _1 acks
      $ M.empty

  in
    concatSnd [ (addr, buildPackets frameNr acks updates events)
              | (addr,(acks,updates,events)) <- msgsPerAddr
              ]

buildPackets :: FrameNr -> [AckID] -> [UpdateMsg] -> [EventMsg] -> [Packet]
buildPackets = buildPackets' (msgLength - packetSerializationOverhead)

-- | Exposes the maxSize parameter for testing purposes.
buildPackets' :: Int -> FrameNr -> [AckID] -> [UpdateMsg] -> [EventMsg] -> [Packet]
buildPackets' _       _       []   []   []  = []
buildPackets' maxSize frameNr acks upds evs = filter (not . packetIsEmpty) (p : ps)
  where
    (ps,(p,_)) = flip State.runState (emptyPacket frameNr, maxSize) $ do
      ps1 <- writeAcks maxSize frameNr acks
      ps2 <- writeUpdates maxSize frameNr upds
      ps3 <- writeEvents maxSize frameNr evs
      return (ps1 ++ ps2 ++ ps3)

-- | Write a list of messages to packets, splitting over multiple
-- packets if they don't fit into one.
writeMsgs :: SerializedSize msg
          => Int
          -> FrameNr
          -> ASetter Packet Packet a [msg]
          -> [msg]
          -> State (Packet,Int) [Packet]
writeMsgs _       _       _    []   = return []
writeMsgs maxSize frameNr lens msgs = addToPacket lens msgs >>= \case
  []        -> return []
  remainder -> do
    (packet,_) <- State.get
    State.put (emptyPacket frameNr, maxSize)
    fmap (packet:) (writeMsgs maxSize frameNr lens remainder)

-- | Some specializations of writeMsgs:
  
writeAcks :: Int -> FrameNr -> [AckID] -> State (Packet,Int) [Packet]
writeAcks maxSize frameNr acks = writeMsgs maxSize frameNr packetAcks acks

writeUpdates :: Int -> FrameNr -> [UpdateMsg] -> State (Packet,Int) [Packet]
writeUpdates maxSize frameNr upds = writeMsgs maxSize frameNr packetUpdates upds

writeEvents :: Int -> FrameNr -> [EventMsg] -> State (Packet,Int) [Packet]
writeEvents maxSize frameNr evs = writeMsgs maxSize frameNr packetEvents evs

-- | Insert some messages into a packet. Pass in a lens to select the right field.
addToPacket :: SerializedSize b
            => ASetter Packet Packet a [b]
            -> [b]
            -> State (Packet,Int) [b]
addToPacket packetLens xs = do
  (packet, spaceLeft) <- State.get
  let cumulativeSpaceLeft = tail $ scanl (-) spaceLeft (map serializedSize xs)
      (toAdd, remainder)  = span ((>=0) . snd) (xs `zip` cumulativeSpaceLeft)
      newSpaceLeft        = snd (last toAdd)
  State.put (set packetLens (map fst toAdd) packet, newSpaceLeft)
  return (map fst remainder)

  
-- | The number of bytes the Packet encoding adds to the bytestring encoding.
-- TODO fragile
packetSerializationOverhead :: Int
packetSerializationOverhead = BS.length $ encode $ Packet 0 [] [] []

-- | Empty packets indicate connection requests.
acksAndSenders :: [(Packet,addr)] -> [(AckID,addr)]
acksAndSenders = concat
  . map ((\(xs,y) -> xs `zip` repeat y) . over _1 _packetAcks)

-- | Extracts update messages from packets, along with the frame number
-- of the packet and the address of the sender.
updatesAndSenders :: [(Packet,addr)] -> [((FrameNr,UpdateMsg),addr)]
updatesAndSenders = concat . map (catOnSnd . extract)
  where
    extract (p,addr) = (_packetFrameNr p, _packetUpdates p, addr)
    catOnSnd (x,ys,z) = (repeat x `zip` ys) `zip` repeat z
    
eventsAndSenders :: [(Packet,addr)] -> [(EventMsg,addr)]
eventsAndSenders = concatMap ((\(xs,y) -> xs `zip` repeat y) . over _1 _packetEvents)

eventAcks :: [(addr,[EventMsg])] -> [(addr,AckID)]
eventAcks xs = [(addr, id) | (addr, events) <- xs, EventMsg id _ <- events]

containsStartGameMsg :: [EventMsg] -> Bool
containsStartGameMsg events = not $ null [() | EventMsg _ StartGameMsg <- events]

-- | Sort packets per sender. TODO might be a bit slow.
packetsPerAddr :: Ord addr => [(Packet,addr)] -> [(addr, [Packet])]
packetsPerAddr = map (\l@((addr,_):_) -> (addr, map snd l))
  . groupBy ((==) `on` fst) . sortOn fst . map swap

splitPackets :: [Packet] -> ([AckID], [(FrameNr, UpdateMsg)], [EventMsg])
splitPackets
  = foldr
    (\(Packet frameNr packetAcks packetUpdates packetEvents) (acks,updates,events)
     -> (packetAcks ++ acks, map (frameNr,) packetUpdates ++ updates, packetEvents ++ events))
    ([],[],[])

unzipAddressedPackets
  :: [(a, [Packet])] -> ([(a,[AckID])], [(a,[(FrameNr,UpdateMsg)])], [(a,[EventMsg])])
unzipAddressedPackets = unzip3 . map (distribute . fmap splitPackets)
  where
    distribute (a, (bs,cs,ds)) = ((a,bs), (a,cs), (a,ds))

-- | Unsafe, use only for debugging!
unsafeDecode :: Serialize a => ByteString -> a
unsafeDecode s = let Right a = decode s in a

--------------------------------------------------------------------------------
-- TESTS
--------------------------------------------------------------------------------

maxMsgSize :: Int -> [AckID] -> [UpdateMsg] -> [EventMsg] -> Int
maxMsgSize maxSize acks updates events = abs maxSize + largestMsgSize
  where
    largestMsgSize
      =  fromMaybe 0
      $  safeMaximum
      $  map serializedSize acks
      ++ map serializedSize updates
      ++ map serializedSize events
  
prop_packetsDontExceedMaxSize :: Int -> [AckID] -> [UpdateMsg] -> [EventMsg]
  -> Bool
prop_packetsDontExceedMaxSize maxSize_ acks updates events
  = all (\p -> BS.length (encode p) <= maxSize + packetSerializationOverhead)
        (buildPackets' maxSize 0 acks updates events)
  where
    -- packet size should be big enough to hold the largest message
    maxSize = maxMsgSize maxSize_ acks updates events

-- | Verify whether no information is dropped or duplicated.
prop_buildPacketNoDropOrDuplication :: Int -> [AckID] -> [UpdateMsg] -> [EventMsg]
  -> Property 
prop_buildPacketNoDropOrDuplication maxSize_ acks updates events
  = Test.QuickCheck.label ("length packets = " ++ show (length packets))
  $ sort acks'    == sort acks
  && sort updates' == sort updates
  && sort events'  == sort events
  where
    acks'    = map fst (acksAndSenders packets)
    updates' = map (\((_,m),_) -> m) (updatesAndSenders packets)
    events'  = map fst (eventsAndSenders packets)
    packets  = buildPackets' maxSize 0 acks updates events `zip` repeat ()
    maxSize  = maxMsgSize maxSize_ acks updates events

-- | buildPackets should never produce empty packets.
prop_noEmptyPackets :: Int -> [AckID] -> [UpdateMsg] -> [EventMsg]
  -> Bool
prop_noEmptyPackets maxSize_ acks updates events
  = none packetIsEmpty (buildPackets' maxSize 0 acks updates events)
  where
    -- packet size should be big enough to hold the largest message
    maxSize  = maxMsgSize maxSize_ acks updates events

-- | buildPackets should not split the data over more packets than necessary.
-- (for our definition of necessary; we don't try to find the absolute optimal split).
prop_buildPackets_noUnnecessarySplits
  :: Int -> [AckID] -> [UpdateMsg] -> [EventMsg]
  -> Bool
prop_buildPackets_noUnnecessarySplits maxSize_ acks updates events
  = length packets == predictNrOfPackets msgSizes
  where
    packets = buildPackets' maxSize 0 acks updates events
    maxSize = maxMsgSize maxSize_ acks updates events
    msgSizes
      = concat [map serializedSize acks, map serializedSize updates, map serializedSize events]
    
    predictNrOfPackets []    = 0
    predictNrOfPackets sizes
      = (+1)
      . predictNrOfPackets
      . map snd
      . dropWhile ((<= maxSize) . fst)
      $ tail (scanl (+) 0 sizes) `zip` sizes

runTests :: IO ()
runTests = sequence_ [ quickCheck prop_packetsDontExceedMaxSize
                     , quickCheck prop_buildPacketNoDropOrDuplication
                     , quickCheck prop_noEmptyPackets
                     , quickCheck prop_buildPackets_noUnnecessarySplits
                     ]


-- TODO any way to automate these?
instance Arbitrary AckID where
  arbitrary = fmap AckID arbitrary

instance Arbitrary PlayerUpdate where
  arbitrary = liftA3 PlayerUpdate arbitrary arbitrary arbitrary
  
instance Arbitrary UpdateMsg where
  arbitrary = oneof [fmap PlayerUpdateMsg arbitrary]

instance Arbitrary EventMsg where
  arbitrary = undefined
  -- oneof [ liftA2 SpawnPlayerMsg arbitrary arbitrary
  --       , liftA2 SpawnBulletMsg arbitrary arbitrary
  --       ]
