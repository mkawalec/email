module Persist.RelatedEmails (persistRelatedEmails) where

import Data.UUID.Types (UUID)
import qualified Network.Mail.Parse.Types as MPT
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Control.Monad (void)
import qualified Data.Map.Strict as M

import Persist.Types
import Types

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
