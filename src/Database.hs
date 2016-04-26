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
import qualified Network.Mail.Parse.Types as MPT

import qualified Pipes.Prelude as P
import qualified Pipes as P
import qualified Debug.Trace as DT

saveMessage :: Connection ->
               ((Either MPT.ErrorMessage MPT.EmailMessage), Metadata) ->
               IO ()
saveMessage conn (msg, metadata) = do
  let uid = find (isUID) metadata
  if isRight msg && isJust uid
    then DT.traceShow uid $ do
      let (IMAP.UID unpackedUid) = fromJust uid
      execute conn
              "insert into message (uid, message) values (?, ?)"
              (unpackedUid, toJSON . fromRight $ msg)
      return ()
    else return ()

saveMessages :: Connection ->
                P.Consumer ((Either MPT.ErrorMessage MPT.EmailMessage), Metadata) IO ()
saveMessages conn = P.mapM_ $ saveMessage conn
