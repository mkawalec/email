module Database (saveMessages) where

import Types
import Data.Aeson (toJSON)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
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
import qualified Data.Aeson.Types as AT
import Control.Monad (void, when)
import Safe

type EmailIdMap = M.Map T.Text UUID

-- |Sequentially saves a set of messages or
-- does nothing if there was a parse error
saveMessages :: Connection ->
                P.Consumer ((Either MPT.ErrorMessage MPT.EmailMessage), Metadata) IO ()
saveMessages conn = P.mapM_ $ saveMessage conn


-- |Saves a message and all of it's related metadata in a single transaction
saveMessage :: Connection ->
               ((Either MPT.ErrorMessage MPT.EmailMessage), Metadata) ->
               IO ()
saveMessage conn (msg, metadata) = do
  let uid = find (isUID) metadata
  if isRight msg && isJust uid
    then do
      idsMap <- saveMetadata conn $ fromRight msg
      let (IMAP.UID unpackedUid) = fromJust uid
      withTransaction conn $ do
        msgIds <- persistMessage conn idsMap (fromRight msg) unpackedUid

        let msgId = headMay msgIds
        when (isJust msgId) $ do
            let unpackedId = (\(Only uuid) -> uuid) $ fromJust msgId

            persistRelatedEmails conn unpackedId idsMap $ fromRight msg
            persistReferences conn unpackedId (fromRight msg)
    else return ()

-- |Saves message references to the db
persistReferences :: Connection -> UUID -> MPT.EmailMessage -> IO ()
persistReferences conn msgId msg = void $ executeMany conn [sql|
    INSERT INTO message_references
    (message_id, references_id)
    VALUES (?, ?)
    |] $ map (\ref -> (msgId, ref)) refs
  where refs = concatMap extractReferences headers
        headers = MPT.emailHeaders msg


extractReferences :: MPT.Header -> [MPT.MessageId]
extractReferences = \case
  MPT.References refs -> refs
  _ -> []


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

persistRelatedEmails :: Connection -> UUID -> EmailIdMap -> MPT.EmailMessage -> IO ()
persistRelatedEmails conn msgId idMap emailMessage = void $
  executeMany conn [sql|
    INSERT INTO message_emails
    (message_id, email_id, relation_type)
    VALUES (?, ?, ?)
  |] serializedAddrs
    where headers = MPT.emailHeaders emailMessage
          serializedAddrs = concatMap (extractAddrs msgId idMap) headers

extractAddrs :: UUID ->
                EmailIdMap ->
                MPT.Header ->
                [(UUID, UUID, RelationType)] -- messageId, emailId, relationType
extractAddrs msgId idMap header = case header of
    MPT.To addrs -> map (serializeAddr TO) addrs
    MPT.CC addrs -> map (serializeAddr CC) addrs
    MPT.BCC addrs -> map (serializeAddr BCC) addrs
    _ -> []
  where addrToId (MPT.EmailAddress addr _) = idMap M.! addr
        serializeAddr addrType addr = (msgId, addrToId addr, addrType)


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

-- |Prepares other tables to be ready for our current message
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

-- |Transforms an email address into a DB-saveable format
saveableAddress :: MPT.EmailAddress -> (T.Text, Maybe T.Text)
saveableAddress (MPT.EmailAddress addr label) = (addr, label)

-- |Get all email addresses that exist in message metadata
getSaveableAddresses :: MPT.EmailMessage -> [MPT.EmailAddress]
getSaveableAddresses msg = concatMap extractHeader (MPT.emailHeaders msg)

extractHeader :: MPT.Header -> [MPT.EmailAddress]
extractHeader header = case header of
  MPT.From addr -> [addr]
  MPT.ReplyTo addr -> [addr]
  MPT.To addrs -> addrs
  MPT.CC addrs -> addrs
  MPT.BCC addrs -> addrs
  _ -> []


-- ALL HAIL THE GLORIOUS BOILERPLATE! GLORY TO THE BOILERPLATE GODS!
isFrom (MPT.From _) = True
isFrom _ = False

isDate (MPT.Date _) = True
isDate _ = False

isReplyTo (MPT.ReplyTo _) = True
isReplyTo _ = False

isMessageId (MPT.MessageId _) = True
isMessageId _ = False

isInReplyTo (MPT.InReplyTo _) = True
isInReplyTo _ = False

isSubject (MPT.Subject _) = True
isSubject _ = False
