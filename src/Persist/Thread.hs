module Persist.Thread (persistThread) where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import qualified Network.Mail.Parse.Types as MPT
import Data.UUID.Types (UUID)
import Control.Logging (warn)
import Control.Applicative

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (when, void)
import qualified Data.UUID as UUID
import qualified Data.Map.Strict as M
import Safe

persistThread :: Connection -> UUID -> MPT.EmailMessage -> IO ()
persistThread conn msgUuid msg = withTransaction conn $ do
  let relatedEmailIds = concatMap (extractRelatedEmailIds) $ MPT.emailHeaders msg
  msgThreadId <- getThreadId conn msgUuid relatedEmailIds

  void $ execute conn [sql|
    UPDATE message SET thread = ? WHERE id = ?
  |] (msgThreadId, msgUuid)


-- |Extacts all message ids of messages related to this message
extractRelatedEmailIds :: MPT.Header -> [MPT.MessageId]
extractRelatedEmailIds = \case
  MPT.References refs -> refs
  MPT.InReplyTo msgId -> [msgId]
  _ -> []

-- |Checks if any of related emails is in a thread
getThreadId :: Connection -> UUID -> [MPT.MessageId] -> IO UUID
getThreadId conn uuid msgIds = do
  threads <- query conn [sql|
    SELECT thread, id FROM message WHERE message_id IN ? AND thread IS NOT NULL
  |] $ Only . In $ msgIds

  let threadIds = M.fromListWith (++) (map (\(t, i) -> (t, [i])) threads)
      uniqueIds = M.keys threadIds

  if length uniqueIds > 0
    then do
      let chosenThreadId = head uniqueIds
      mapM_ (settleThreadIds conn chosenThreadId) $ M.elems threadIds
      return chosenThreadId
    else createThread conn

-- |Sets a `threadId` for messages with given uuids. Needed when some messages
--  in a thread have a different thread id then the rest
settleThreadIds :: Connection -> UUID -> [UUID] -> IO ()
settleThreadIds conn threadId messageIds = void $ execute conn [sql|
  UPDATE message SET thread = ? WHERE id IN ?
|] (threadId, In messageIds)

createThread :: Connection -> IO UUID
createThread conn = fromOnly . head <$> query_ conn [sql|
  INSERT INTO thread (id) VALUES (DEFAULT) RETURNING id
|]
