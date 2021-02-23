-- TODO: key mapping should return (Maybe k1) in stead of k1.

--------------------------------------------------------------------------------
module KeyMap  where

import Data.Map (Map)
import qualified Data.Map as M
import Data.ConfigFile
import Data.Tuple (swap)
import System.IO ()
import Text.Read (readMaybe)
--------------------------------------------------------------------------------

type KeyReader k = (String -> Maybe k)

data KeyMapError = CfgParseError CPError | KeyReadError String
    deriving (Show, Eq)

-- Read cfg file containing keybindings and construct a key mapping function.
constructKeyMap :: (Ord k1, Read k1, Read k2)
                => FilePath
                -> IO (k1 -> Maybe k2)
constructKeyMap filename = do
    cfgparser <- mapLeft CfgParseError <$> readfile emptyCP filename
    case (cfgparser >>= readBindingsFromCfg)
      of Left   err -> (ioError . userError . show) err
         Right  as  -> (return . flip M.lookup . M.fromList) as


readBindingsFromCfg :: (Read k1, Read k2)
                    => ConfigParser
                    -> Either KeyMapError [(k1,k2)]
readBindingsFromCfg cp = do
  itemList <- mapLeft CfgParseError $ items cp "keys"
  let (k2strs, k1strs) = unzip itemList
  let kread :: Read k => String -> Either KeyMapError k
      kread s = (provideErrorContext (KeyReadError s) . readMaybe) s
  k1s <- sequence $ map kread k1strs
  k2s <- sequence $ map kread k2strs
  return (zip k1s k2s)


provideErrorContext :: b -> Maybe a -> Either b a
provideErrorContext err = maybe (Left err) Right

mapLeft :: (b -> c) -> Either b a -> Either c a
mapLeft f (Left x)  = Left (f x)
mapLeft _ (Right x) = Right x
