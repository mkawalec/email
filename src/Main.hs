module Main where

import Network.HaskellNet.IMAP
import Network.HaskellNet.IMAP.Connection
import Network.HaskellNet.IMAP.SSL
import Control.Monad
import Data.IORef
import Control.Concurrent.STM
import Control.Concurrent (forkIO)

import Data.Either (isRight, rights, lefts)

import LoadEnv (loadEnv)
import System.Environment (getEnv)
import qualified Data.ByteString.Char8 as BS

import Types
import EmailParser.Types
import EmailParser (parseMessage)
import System.TimeIt

connSettings = defaultSettingsIMAPSSL {
  sslPort = 993
}

notifier :: TChan UpdateMessage -> IO ()
notifier updateChannel = do
  val <- atomically $ readTChan updateChannel
  putStrLn . show $ val
  notifier updateChannel

printBody :: EmailBody -> IO ()
printBody body = case body of
  TextBody text -> putStrLn "text"
  MIMEBody headers body -> putStrLn "MIME"
  Attachment name body -> putStrLn ("got a " ++ show name)
  _ -> putStrLn "not text"

main :: IO ()
main = do
  notificationChan <- newTChanIO
  forkIO $ notifier notificationChan

  {-imapConn <- connectIMAPSSLWithSettings "imap.gmail.com" connSettings

  loadEnv
  username <- getEnv "USERNAME"
  password <- getEnv "PASSWORD"
  login imapConn username password

  select imapConn "INBOX"
  messageIds <- search imapConn [ALLs]


  mails <- mapM (fetch imapConn) messageIds
  writeFile "data" (show mails)-}

  msgs <- readFile "data" >>= \x -> return (read x :: [BS.ByteString])
  messages <- timeIt $ mapM parseMessage msgs

  --messages <- mapM (parseMessage . fetch imapConn) messageIds
  let allBodies = concat $ map emailBodies (rights messages)
  mapM_ printBody (allBodies)

  putStrLn (show  . length $ rights messages)
  putStrLn (show . length . lefts $ messages)
  --mapM_ (putStrLn . show . length . emailBodies) (rights messages)
  --mapM_ (putStrLn . show) (take 1 messages

  putStrLn "done"
