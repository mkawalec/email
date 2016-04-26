module Fetch where

import qualified Network.IMAP.Types as I
import Network.IMAP
import Types
import Control.Monad (liftM, sequence)
import qualified Data.Text as T
import qualified Network.Mail.Parse as MP
import qualified Network.Mail.Parse.Types as MPT
import qualified Data.List as L
import Data.Either.Utils (maybeToEither)
import qualified Pipes.Prelude as P
import qualified Pipes as P
import Pipes ((>->), lift)
import Network.Connection (connectionPut, connectionGetChunk')
import Control.Monad.IO.Class (liftIO)
import qualified Debug.Trace as DT
import qualified ListT as LT

getMessageIds :: I.IMAPConnection -> IO (Either Error [UID])
getMessageIds conn = unpack allIds
  where unpackSearch = \(I.Search nums) -> nums
        allIds = simpleFormat $ uidSearch conn "ALL"
        unpack = (liftM . liftM) (concat . map unpackSearch . filter I.isSearch)

getMessages :: I.IMAPConnection -> [UID] -> P.Producer I.CommandResult IO ()
getMessages conn uids = LT.traverse_ (P.yield) fetchMessages
  where uidsToQuery = T.intercalate "," $ map (T.pack . show) uids
        fetchMessages = (uidFetch conn uidsToQuery :: LT.ListT (P.Producer I.CommandResult IO) I.CommandResult)

parseMsg :: P.Pipe I.CommandResult ((Either MPT.ErrorMessage MPT.EmailMessage), Metadata) IO ()
parseMsg = P.map (\a -> DT.trace "msg" a) >-> preserveNeeded >-> parseMessage

preserveNeeded :: P.Pipe I.CommandResult I.UntaggedResult IO ()
preserveNeeded = P.filter (\el-> I.isUntagged el)
               >-> P.filter (\(I.Untagged res) -> I.isFetch res)
               >-> P.map (\(I.Untagged res) -> res)

parseMessage :: P.Pipe I.UntaggedResult ((Either MPT.ErrorMessage MPT.EmailMessage), Metadata) IO ()
parseMessage = do
  (I.Fetch msg) <- P.await
  let (bodies, metadata) = L.partition (I.isBody) msg
  DT.traceShow (length bodies) $ mapM_ (\(I.Body b) -> P.yield $! (MP.parseMessage b, metadata)) bodies
  parseMessage

instance I.Universe (LT.ListT (P.Producer I.CommandResult IO)) where
  connectionPut' c d = liftIO $ connectionPut c d
  connectionGetChunk'' c cont = liftIO $ connectionGetChunk' c cont
