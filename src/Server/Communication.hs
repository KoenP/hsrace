module Server.Communication where

import FRP.Elerea.Param
import Network.Socket.ByteString
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Serialize
import Control.Concurrent.STM.TChan
import Control.Applicative
import Data.List

import Network.Packet
import Network.UpdateMsg
import Network.EventMsg
import Network.Setup
import ElereaTools
import Basket
import TypeDefs
import IDMap (ID)

-- Note: input may contain duplicates, or addresses
-- that are already in the table.
playerAddressTable :: (Eq sockAddr, ID playerID)
                   => Signal [sockAddr]
                   -> SG (Signal [(playerID,sockAddr)],
                          Signal [playerID],
                          Signal [sockAddr])
playerAddressTable newAddrs = mdo
  table'     <- delay [] table
  let addrs = fmap (map snd) table'
  newEntries <- assignIDs $ liftA2 (\\) (fmap nub newAddrs) addrs
  table      <- memo $ liftA2 (++) newEntries table'
  return (table, fmap (map fst) newEntries, fmap (map snd) newEntries)
  
readMessages :: TChan (ByteString, sockAddr) -> IO [(Packet, sockAddr)]
readMessages recvChan = do
  rawInputs <- atomically (flushTChan recvChan)
  return [ (handleDecodeError (decode msg), addr)
         | (msg,addr) <- rawInputs
         ]
  where
    handleDecodeError (Right msg) = msg
    handleDecodeError (Left err) = error $ show err

sendMessages :: TChan (ByteString, sockAddr)
             -> [(Packet, sockAddr)]
             -> IO ()
sendMessages sendChan messages
  = atomically
  $ writeListTChan sendChan [(encode stc, addr) | (stc, addr) <- messages]
