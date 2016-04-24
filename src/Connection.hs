module Connection (getConnection) where

import Network.Connection
import Network.IMAP
import Network.IMAP.Types
import Config (readConfig)
import Types
import Data.Yaml
import Control.Monad.IO.Class (liftIO)
import Data.Maybe

getConnection :: AccountConfig -> IO IMAPConnection
getConnection acc = do
  let tls = TLSSettingsSimple False False False
  let params = ConnectionParams "imap.gmail.com" 993 (Just tls) Nothing
  conn <- connectServer params Nothing

  login conn (accountLogin acc) (accountPassword acc)
  return conn
