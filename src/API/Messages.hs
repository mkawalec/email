module API.Messages (messagesAPI, MessagesAPI, server) where

import qualified Data.Text as T
import Types
import qualified Network.Mail.Parse.Types as MPT
import Servant
import Data.Aeson (ToJSON, decode)
import Data.Aeson.Types (Value(..), toJSON)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import GHC.Generics
import Control.Monad.IO.Class (liftIO)
import Control.Applicative
import Data.Time.LocalTime (ZonedTime)
import Data.UUID.Types (UUID)
import qualified Data.UUID as UUID
import Data.Maybe (catMaybes)

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as M

type MessagesAPI = "messages" :> Get '[JSON] [MessageDigest]

type EmailRelationMap = M.Map UUID [EmailWithRelation]
type EmailMap = M.Map UUID MPT.EmailAddress

data MessageDigest = MessageDigest {
  msgId :: !UUID
, uid :: !Int
, fromAddr :: Maybe MPT.EmailAddress
, sentDate :: Maybe ZonedTime
, replyTo :: Maybe MPT.EmailAddress
, messageId :: Maybe T.Text
, inReplyTo :: Maybe T.Text
, subject :: Maybe T.Text
, to :: [MPT.EmailAddress]
, cc :: [MPT.EmailAddress]
, bcc :: [MPT.EmailAddress]
} deriving (Eq, Show, Generic)

instance ToJSON UUID where
  toJSON = String . UUID.toText

data EmailWithRelation = EmailWithRelation RelationType !MPT.EmailAddress

messagesAPI :: Proxy MessagesAPI
messagesAPI = Proxy

instance ToJSON MessageDigest

server :: Connection -> Server MessagesAPI
server conn = liftIO $ do
  messages <- query_ conn [sql|
    SELECT id, uid, from_addr, sent_date, reply_to, message_id, in_reply_to,
    subject FROM message
  |]
  emailMaps <- fetchEmails conn (messageIds messages) (emailsInMessage messages)
  return $! map (uncurry deserializeMessage emailMaps) messages


fetchEmails :: Connection -> [UUID] -> Set UUID -> IO (EmailRelationMap, EmailMap)
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

  return (emailsByMessageId, emailsByEmailId)

deserializeMessage :: EmailRelationMap ->
                      EmailMap ->
                      (UUID, Int, Maybe UUID, Maybe ZonedTime,
                        Maybe UUID, Maybe T.Text, Maybe T.Text, Maybe T.Text) ->
                      MessageDigest
deserializeMessage emailsByMessageId emailsByEmailId (msgId, uid, fromUid, sentDate,
  replyToUid, messageId, inReplyTo, subject) =
    MessageDigest msgId
                  uid
                  (fromUid >>= flip M.lookup emailsByEmailId)
                  sentDate
                  (replyToUid >>= flip M.lookup emailsByEmailId)
                  messageId
                  inReplyTo
                  subject
                  (findEmails TO)
                  (findEmails CC)
                  (findEmails BCC)
      where relatedEmails = emailsByMessageId M.! msgId
            byRelation relation (EmailWithRelation rel _) = relation == rel
            unpack (EmailWithRelation _ email) = email
            findEmails relation = map unpack $ filter (byRelation relation) relatedEmails


relatedToMap :: [(UUID, T.Text, Maybe T.Text, RelationType)] -> EmailRelationMap
relatedToMap = M.fromListWith (++) . map toKeyVal
  where toKeyVal (uuid, email, label, relation) =
          (uuid, [EmailWithRelation relation $ MPT.EmailAddress email label])

emailsToMap :: [(UUID, T.Text, Maybe T.Text)] -> EmailMap
emailsToMap = M.fromList . map (\(uuid, email, label) ->
  (uuid, MPT.EmailAddress email label))

emailsInMessage :: [(UUID, Int, Maybe UUID, Maybe ZonedTime,
  Maybe UUID, Maybe T.Text, Maybe T.Text, Maybe T.Text)] -> Set UUID
emailsInMessage messages = Set.fromList $ catMaybes allEmails
    where allEmails = concatMap (\(_, _, e1, _, e2, _, _, _) -> [e1, e2]) messages

messageIds :: [(UUID, Int, Maybe UUID, Maybe ZonedTime,
  Maybe UUID, Maybe T.Text, Maybe T.Text, Maybe T.Text)] -> [UUID]
messageIds messages = map (\(msgId, _, _, _, _, _, _, _) -> msgId) messages
