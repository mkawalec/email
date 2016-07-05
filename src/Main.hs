module Main where

import Connection
import Discover
import Types
import Config
import Fetch
import Database
import Data.Either.Unwrap (fromRight)
import Control.Monad (liftM)
import Data.Either (isRight)
import Network.IMAP
import qualified Debug.Trace as DT

import Pipes ((>->))
import qualified Pipes.Prelude as P
import Pipes.Core (runEffect)
import Database.PostgreSQL.Simple (connectPostgreSQL)
import API
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  firstAccount <- (liftM . liftM) (head . accounts) readConfig
  dbConn <- DT.trace "startup" $ connectPostgreSQL "postgresql://email:email@127.0.0.1:2345/email"

  --run 8085 $ api dbConn

  if isRight firstAccount
    then do
      conn <- DT.trace "getconn" $ getConnection (fromRight firstAccount)
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
    else DT.trace "left" $ return ()
