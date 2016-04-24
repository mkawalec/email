module Main where

import Connection
import Discover
import Types
import Config
import Fetch
import Data.Either.Unwrap (fromRight)
import Control.Monad (liftM)
import Data.Either (isRight)
import Network.IMAP

main :: IO ()
main = do
  firstAccount <- (liftM . liftM) (head . accounts) readConfig
  if isRight firstAccount
    then do
      conn <- getConnection (fromRight firstAccount)
      getAllMailboxes conn >>= print
      select conn "INBOX"

      ids <- getMessageIds conn
      if isRight ids
        then getMessages conn (fromRight ids) >>= print
        else return ()
    else return ()
