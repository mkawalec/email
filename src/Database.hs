module Database (saveMessages) where

import Types
import Data.Aeson (toJSON)
import Database.PostgreSQL.Simple
import Data.Either (isRight)
import Data.Either.Utils (fromRight)
import Data.List (find)
import Network.IMAP.Types (isUID)
import qualified Network.IMAP.Types as IMAP
import Data.Maybe (isJust, fromJust)
import qualified Data.Text as T
import qualified Network.Mail.Parse.Types as MPT

import qualified Pipes.Prelude as P
import qualified Pipes as P
import qualified Debug.Trace as DT
import Data.UUID.Types (UUID)
import Data.Time.LocalTime (ZonedTime)

import qualified Data.Map.Strict as M

saveMessage :: Connection ->
               ((Either MPT.ErrorMessage MPT.EmailMessage), Metadata) ->
               IO ()
saveMessage conn (msg, metadata) = do
  let uid = find (isUID) metadata
  if isRight msg && isJust uid
    then DT.traceShow (MPT.emailHeaders $ fromRight msg) $ do
      msgIds <- saveMetadata conn $ fromRight msg
      let (IMAP.UID unpackedUid) = fromJust uid
      execute conn
              "insert into message (uid, message) values (?, ?)"
              (unpackedUid, toJSON . fromRight $ msg)
      return ()
    else return ()

serializeMessage :: Connection ->
                    M.Map T.Text UUID ->
                    MPT.EmailMessage ->
                    Int ->
                    (Int, UUID, ZonedTime, Maybe UUID, Maybe T.Text,
                    Maybe T.Text, Maybe T.Text, T.Text)

-- |Prepares other tables to be ready for our current message
--  returns a map mapping email addresses to their ids from the DB
saveMetadata :: Connection -> MPT.EmailMessage -> IO (M.Map T.Text UUID)
saveMetadata conn msg = do
  let saveableAddresses = getSaveableAddresses msg

  saveEmails conn saveableAddresses
  getEmailIds conn saveableAddresses

-- |Fetches a list of ids for a list of emails
getEmailIds :: Connection -> [MPT.EmailAddress] -> IO (M.Map T.Text UUID)
getEmailIds conn addrs = do
  ids <- query conn [sql|
    SELECT address, id FROM email_address WHERE address IN ?
  |] map (MPT.emailAddress) addrs
  return $ M.fromList ids

saveEmails :: Connection -> [MPT.EmailAddress] -> IO ()
saveEmails conn addrs =
  executeMany conn [sql|
    INSERT INTO email_address (address, label) values (?, ?)
    ON CONFLICT DO NOTHING
  |] $ map saveableAddress addrs

-- |Transforms an email address into a DB-saveable format
saveableAddress :: MPT.EmailAddress -> (Text, Maybe Text)
saveableAddress (MPT.EmailAddress addr label) = (addr, label)

-- |Get all email addresses that exist in message metadata
getSaveableAddresses :: MPT.EmailMessage -> [MPT.EmailAddress]
getSaveableAddresses msg = foldr foldHeader [] (emailHeaders msg)

foldHeader :: MPT.Header -> [MPT.EmailAddress] -> [MPT.EmailAddress]
foldHeader header addresses = case header of
  From addr -> addr:addresses
  ReplyTo addr -> addr:addresses
  To addrs -> addrs ++ addresses
  CC addrs -> addrs ++ addresses
  BCC addrs -> addrs ++ addresses
  _ -> addresses


saveMessages :: Connection ->
                P.Consumer ((Either MPT.ErrorMessage MPT.EmailMessage), Metadata) IO ()
saveMessages conn = P.mapM_ $ saveMessage conn
