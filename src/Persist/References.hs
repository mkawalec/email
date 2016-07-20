module Persist.References (persistReferences) where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Data.UUID.Types (UUID)
import qualified Network.Mail.Parse.Types as MPT
import Control.Monad (void)


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
