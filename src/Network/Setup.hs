module Network.Setup
  ( TChan
  , ByteString
  , SockAddr
  , atomically
  , setupSocket
  , launchSender
  , launchListener
  , flushTChan
  , writeTChan
  , writeListTChan
  , encode
  , decode
  , hints
  ) where

import Data.Functor ((<$>))
import qualified Data.ByteString as BS
import Control.Monad (forever, forM_)
import Network.Socket
import Network.Socket.ByteString
import Data.Serialize (encode, decode)
import Data.ByteString (ByteString)
import Control.Monad.STM (STM, atomically)
import Control.Concurrent.STM.TChan ( TChan
                                    , newTChan
                                    , readTChan
                                    , tryReadTChan
                                    , writeTChan
                                    )
import Control.Concurrent (forkIO)

import Constants

setupSocket :: String -> IO Socket
setupSocket port = do
  myAddr:_ <- getAddrInfo
                  (Just (defaultHints { addrFlags = [AI_PASSIVE]
                                      , addrSocketType = Datagram
                                      }))
                  Nothing
                  (Just port)
  sock <- socket (addrFamily myAddr) Datagram defaultProtocol
  bind sock (addrAddress myAddr)
  return sock

launchSender :: Socket -> IO (TChan (ByteString, SockAddr))
launchSender sock = do
  sendChan <- atomically newTChan
  _ <- forkIO $ forever $ do
    (msg, addr) <- atomically (readTChan sendChan) -- blocking read
    (if BS.length msg > msgLength
     then error "message too long"
     else sendTo sock msg addr)
  return sendChan

launchListener :: Socket -> IO (TChan (ByteString, SockAddr))
launchListener sock = do
  recvChan <- atomically newTChan
  _ <- forkIO $ forever $ do msg <- recvFrom sock msgLength
                             atomically (writeTChan recvChan msg)
  return recvChan

flushTChan :: TChan a -> STM [a]
flushTChan chan = tryReadTChan chan >>= \case
  Nothing -> return []
  Just x  -> (x:) <$> flushTChan chan

writeListTChan :: TChan a -> [a] -> STM ()
writeListTChan chan xs = forM_ xs (writeTChan chan)

hints :: AddrInfo
hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Datagram }
