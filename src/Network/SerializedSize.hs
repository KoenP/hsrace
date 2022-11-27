module Network.SerializedSize where

import Data.Serialize ( Serialize, encode )
import Data.ByteString (length)

-- | The default implementation just encodes and then computes the length
--   of the encoding. But it can be overridden with a faster implementation
--   (for example if the size is the same for all values of type `a`).
class Serialize a => SerializedSize a where 
    serializedSize :: a -> Int
    serializedSize = Data.ByteString.length . encode