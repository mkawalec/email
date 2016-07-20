module Persist (saveMessages) where

import Types
import Database.PostgreSQL.Simple (Connection)
import qualified Network.Mail.Parse.Types as MPT
import qualified Pipes.Prelude as P
import qualified Pipes as P
import Persist.Message (saveMessage)


-- |Sequentially saves a set of messages or
-- does nothing if there was a parse error
saveMessages :: Connection ->
                P.Consumer ((Either MPT.ErrorMessage MPT.EmailMessage), Metadata) IO ()
saveMessages conn = P.mapM_ $ saveMessage conn
