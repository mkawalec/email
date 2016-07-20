module Persist.Message (saveMessage) where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Data.List (find)
import Data.Maybe (isJust, fromJust)
import Data.Either (isRight)
import Data.Either.Utils (fromRight)
import Data.UUID.Types (UUID)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Time.LocalTime (ZonedTime)

import Data.Aeson (toJSON)
import qualified Data.Aeson.Types as AT

import Control.Monad (void, when)
import Safe

import Types
import Persist.Types
import Persist.Utils
import Persist.RelatedEmails (persistRelatedEmails)
import Persist.References (persistReferences)
import qualified Network.Mail.Parse.Types as MPT
import Network.IMAP.Types (isUID)
import qualified Network.IMAP.Types as IMAP

-- |Saves a message and all of it's related metadata
saveMessage :: Connection ->
               ((Either MPT.ErrorMessage MPT.EmailMessage), Metadata) ->
               IO ()
saveMessage conn (msg, metadata) = do
  let uid = find (isUID) metadata
  if isRight msg && isJust uid
    then do
      idsMap <- saveMetadata conn $ fromRight msg
      let (IMAP.UID unpackedUid) = fromJust uid
      msgIds <- persistMessage conn idsMap (fromRight msg) unpackedUid

      let msgId = headMay msgIds
      when (isJust msgId) $ do
          let unpackedId = (\(Only uuid) -> uuid) $ fromJust msgId

          persistRelatedEmails conn unpackedId idsMap $ fromRight msg
          persistReferences conn unpackedId (fromRight msg)
    else return ()

persistMessage :: Connection ->
                  EmailIdMap ->
                  MPT.EmailMessage ->
                  Int ->
                  IO [(Only UUID)]
persistMessage conn idsMap msg messageUid = query conn [sql|
    INSERT INTO message
    (uid, from_addr, sent_date, reply_to, message_id, in_reply_to, subject, message)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?)
    RETURNING id
    |] serializedMsg
  where serializedMsg = serializeMessage idsMap msg messageUid

-- |Serializes the bulk of the message, along with message contents
serializeMessage :: EmailIdMap ->
                    MPT.EmailMessage ->
                    Int ->
                    (Int, Maybe UUID, Maybe ZonedTime, Maybe UUID, Maybe T.Text,
                    Maybe T.Text, Maybe T.Text, AT.Value) -- uid, from_addr,
                    -- sent_date, reply_to, message_id, in_reply_to, subject,
                    -- message itself
serializeMessage idMap msg uid = (
      uid
    , (\(MPT.From (MPT.EmailAddress addr _)) -> idMap M.! addr) <$> findHeader isFrom
    , (\(MPT.Date time) -> time) <$> findHeader isDate
    , (\(MPT.ReplyTo (MPT.EmailAddress addr _)) -> idMap M.! addr) <$> findHeader isReplyTo
    , (\(MPT.MessageId msgId) -> msgId) <$> findHeader isMessageId
    , (\(MPT.InReplyTo msgId) -> msgId) <$> findHeader isInReplyTo
    , (\(MPT.Subject subject) -> subject) <$> findHeader isSubject
    , toJSON msg
    )
  where findHeader hdrMatch = find hdrMatch $ MPT.emailHeaders msg

-- |Saves all the emails that exist in this message and
--  returns a map mapping email addresses to their ids from the DB
saveMetadata :: Connection -> MPT.EmailMessage -> IO EmailIdMap
saveMetadata conn msg = do
  let saveableAddresses = getSaveableAddresses msg

  saveEmails conn saveableAddresses
  getEmailIds conn saveableAddresses

-- |Fetches a list of ids for a list of emails
getEmailIds :: Connection -> [MPT.EmailAddress] -> IO EmailIdMap
getEmailIds conn addrs = do
  ids <- query conn [sql|
    SELECT address, id FROM email_address WHERE address IN ?
  |] $ Only . In $ map (MPT.emailAddress) addrs
  return $ M.fromList ids

saveEmails :: Connection -> [MPT.EmailAddress] -> IO ()
saveEmails conn addrs = void $
  executeMany conn [sql|
    INSERT INTO email_address (address, label) values (?, ?)
    ON CONFLICT DO NOTHING
  |] $ map saveableAddress addrs
