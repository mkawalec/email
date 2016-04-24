module Main where

import Connection
import Discover
import Types
import Config
import Data.Either.Unwrap (fromRight)
import Control.Monad (liftM)
import Data.Either (isRight)

main :: IO ()
main = do
  firstAccount <- (liftM . liftM) (head . accounts) readConfig
  if isRight firstAccount
    then do
      conn <- getConnection (fromRight firstAccount)
      getAllMailboxes conn >>= print
    else return ()
