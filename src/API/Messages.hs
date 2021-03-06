module API.Messages (messagesAPI, MessagesAPI, server) where

import qualified Data.Text as T
import Types
import qualified Network.Mail.Parse.Types as MPT
import Servant
import Data.Aeson (ToJSON, decode)
import Data.Aeson.Types (toJSON)
import qualified Data.Aeson.Types as JSON
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import GHC.Generics
import Control.Monad.IO.Class (liftIO)
import Control.Applicative
import Data.Time.LocalTime (ZonedTime)
import Data.UUID.Types (UUID)
import qualified Data.UUID as UUID
import qualified Debug.Trace as DT
import Data.Maybe (catMaybes, fromJust, fromMaybe, isNothing, isJust)

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as M
import Data.Tuple.HT (uncurry3)

type MessagesAPI = "messages" :> Get '[JSON] [MessageDigest]
                 :<|> "messages" :> Capture "id" String :> Get '[JSON] MessageDigest

type EmailRelationMap = M.Map UUID [EmailWithRelation]
type EmailMap = M.Map UUID MPT.EmailAddress
type ReferenceMap = M.Map UUID [T.Text]

data MessageDigest = MessageDigest {
  msgId :: !UUID
, uid :: !Int
, fromAddr :: Maybe MPT.EmailAddress
, sentDate :: Maybe ZonedTime
, replyTo :: Maybe MPT.EmailAddress
, messageId :: Maybe T.Text
, references :: [T.Text]
, inReplyTo :: Maybe T.Text
, subject :: Maybe T.Text
, thread :: Maybe UUID
, to :: [MPT.EmailAddress]
, cc :: [MPT.EmailAddress]
, bcc :: [MPT.EmailAddress]
, message :: Maybe JSON.Value
} deriving (Eq, Show, Generic)

instance ToJSON UUID where
  toJSON = JSON.String . UUID.toText

data EmailWithRelation = EmailWithRelation RelationType !MPT.EmailAddress

messagesAPI :: Proxy MessagesAPI
messagesAPI = Proxy

instance ToJSON MessageDigest

server :: Connection -> Server MessagesAPI
server conn = (liftIO $ getMessages conn Nothing) :<|> getMessage conn

getMessage :: Connection -> String -> Handler MessageDigest
getMessage conn msgId =
    if isJust uuid
      then liftIO $ head <$> getMessages conn uuid
      else throwError $ err400 { errBody = "The id provided is not a proper uuid" }
  where uuid = UUID.fromString msgId

getMessages :: Connection -> Maybe UUID -> IO [MessageDigest]
getMessages conn uuid = do
  messages <- if isNothing uuid
    then query_ conn [sql|
          SELECT id, uid, from_addr, sent_date, reply_to, message_id,
          in_reply_to, subject, thread, NULL FROM message
        |]
    else query conn [sql|
          SELECT id, uid, from_addr, sent_date, reply_to, message_id,
          in_reply_to, subject, thread, message FROM message
          WHERE id = ?
        |] (Only $ fromJust uuid)
  emailMaps <- fetchEmails conn (messageIds messages) (emailsInMessage messages)
  return $! map (uncurry3 deserializeMessage emailMaps) messages


fetchEmails :: Connection -> [UUID] -> Set UUID ->
               IO (EmailRelationMap, EmailMap, ReferenceMap)
fetchEmails conn msgIds emailIds = do
  -- Emails from to/cc/bcc fields
  emailsByMessageId <- relatedToMap <$> (query conn [sql|
    SELECT m.message_id, e.address, e.label, m.relation_type
    FROM email_address e, message_emails m
    WHERE m.message_id IN ?
    AND m.email_id = e.id
  |] $ Only . In $ msgIds)

  -- Emails represented by uuid fields in the message itself
  emailsByEmailId <- emailsToMap <$> (query conn [sql|
    SELECT id, address, label
    FROM email_address
    WHERE id IN ?
  |] $ Only . In $ Set.toList emailIds)

  -- The references fields
  referenceMap <- toReferenceMap <$> (query conn [sql|
    SELECT message_id, references_id
    FROM message_references
    WHERE message_id IN ?
  |] $ Only . In $ msgIds)

  return (emailsByMessageId, emailsByEmailId, referenceMap)

deserializeMessage :: EmailRelationMap ->
                      EmailMap ->
                      ReferenceMap ->
                      (UUID, Int, Maybe UUID, Maybe ZonedTime,
                        Maybe UUID, Maybe T.Text, Maybe T.Text,
                        Maybe T.Text, Maybe UUID, Maybe JSON.Value) ->
                      MessageDigest
deserializeMessage emailsByMessageId emailsByEmailId referenceMap (msgId, uid,
  fromUid, sentDate, replyToUid, messageId, inReplyTo, subject, threadId, messageBody) =
    MessageDigest msgId
                  uid
                  (fromUid >>= flip M.lookup emailsByEmailId)
                  sentDate
                  (replyToUid >>= flip M.lookup emailsByEmailId)
                  messageId
                  (fromMaybe [] $ msgId `M.lookup` referenceMap)
                  inReplyTo
                  subject
                  threadId
                  (findEmails TO)
                  (findEmails CC)
                  (findEmails BCC)
                  messageBody
      where relatedEmails = fromMaybe [] $ msgId `M.lookup` emailsByMessageId
            byRelation relation (EmailWithRelation rel _) = relation == rel
            unpack (EmailWithRelation _ email) = email
            findEmails relation = map unpack $ filter (byRelation relation) relatedEmails

toReferenceMap :: [(UUID, T.Text)] -> ReferenceMap
toReferenceMap msgReferences = M.fromListWith (++) asList
  where asList = map (\(uuid, ref) -> (uuid, [ref])) msgReferences

relatedToMap :: [(UUID, T.Text, Maybe T.Text, RelationType)] -> EmailRelationMap
relatedToMap = M.fromListWith (++) . map toKeyVal
  where toKeyVal (uuid, email, label, relation) =
          (uuid, [EmailWithRelation relation $ MPT.EmailAddress email label])

emailsToMap :: [(UUID, T.Text, Maybe T.Text)] -> EmailMap
emailsToMap = M.fromList . map (\(uuid, email, label) ->
  (uuid, MPT.EmailAddress email label))

emailsInMessage :: [(UUID, Int, Maybe UUID, Maybe ZonedTime,
  Maybe UUID, Maybe T.Text, Maybe T.Text, Maybe T.Text,
  Maybe UUID, Maybe JSON.Value)] -> Set UUID
emailsInMessage messages = Set.fromList $ catMaybes allEmails
    where allEmails = concatMap (\(_, _, e1, _, e2, _, _, _, _, _) -> [e1, e2]) messages

messageIds :: [(UUID, Int, Maybe UUID, Maybe ZonedTime,
  Maybe UUID, Maybe T.Text, Maybe T.Text, Maybe T.Text,
  Maybe UUID, Maybe JSON.Value)] -> [UUID]
messageIds messages = map (\(msgId, _, _, _, _, _, _, _, _, _) -> msgId) messages
