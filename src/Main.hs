module Main where

import Connection
import Discover
import Types
import Config
import Fetch
import Persist
import Data.Either.Unwrap (fromRight)
import Control.Monad (liftM, void)
import Data.Either (isRight)
import Network.IMAP
import qualified Debug.Trace as DT

import Pipes ((>->))
import qualified Pipes.Prelude as P
import Pipes.Core (runEffect)
import Database.PostgreSQL.Simple (connectPostgreSQL)
import API
import Network.Wai.Handler.Warp (run)
import Control.Concurrent.Thread (forkIO)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Control.Logging (errorL)

main :: IO ()
main = do
  config <- readConfig
  let firstAccount = head . accounts <$> config
      databaseHost = dbHost <$> config

  connString <- if isRight databaseHost
    then return $ T.concat ["postgresql://email:email@", fromRight databaseHost, ":2345/email"]
    else error "Count not find database host info"

  dbConn <- connectPostgreSQL $ encodeUtf8 connString

  -- The API thread
  (_, apiWait) <- forkIO $ run 8085 $ api dbConn

  if isRight firstAccount
    then do
      conn <- getConnection (fromRight firstAccount)
      getAllMailboxes conn >>= print
      select conn "INBOX"

      ids <- getMessageIds conn
      if isRight ids
        then do
          runEffect $ (getMessages conn (fromRight ids))
                      >-> parseMsg
                      >-> saveMessages dbConn
          return ()
        else return ()
    else error "No account details count be found in the config.yml file"
  void apiWait
