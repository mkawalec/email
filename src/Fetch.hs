module Fetch where

import Network.IMAP.Types
import Network.IMAP
import Types
import Control.Monad (liftM, sequence)
import qualified Data.Text as T
import qualified Network.Mail.Parse as MP
import Network.Mail.Parse.Types
import qualified Network.Mail.Parse.Types as MPT
import Data.List (find)
import Data.Either.Utils (maybeToEither)
import qualified Pipes.Prelude as P
import qualified Pipes as P
import Pipes ((>->), lift)
import Network.Connection (connectionPut, connectionGetChunk')
import Control.Monad.IO.Class (liftIO)
import qualified Debug.Trace as DT
import qualified ListT as LT

parsePipe :: P.Pipe UntaggedResult (Either MPT.ErrorMessage EmailMessage) IO ()
parsePipe = P.map parseMessage

preserveNeeded :: P.Pipe CommandResult UntaggedResult IO ()
preserveNeeded = P.filter (\el-> isUntagged el)
               >-> P.filter (\(Untagged res) -> isFetch res)
               >-> P.map (\(Untagged res) -> res)

parseMsg :: P.Pipe CommandResult (Either MPT.ErrorMessage EmailMessage) IO ()
parseMsg = P.map (\a -> DT.trace "gotmsg" a) >-> preserveNeeded >-> parsePipe

getMessageIds :: IMAPConnection -> IO (Either Error [Int])
getMessageIds conn = unpack allIds
  where unpackSearch = \(Search nums) -> nums
        allIds = simpleFormat $ uidSearch conn "ALL"
        unpack = (liftM . liftM) (concat . map unpackSearch . filter isSearch)

getMessages :: IMAPConnection -> [Int] -> P.Producer CommandResult IO ()
getMessages conn uids = LT.traverse_ (P.yield) (uidFetch conn uidsToQuery :: LT.ListT (P.Producer CommandResult IO) CommandResult)
  where uidsToQuery = T.intercalate "," $ map (T.pack . show) uids

instance Universe (LT.ListT (P.Producer CommandResult IO)) where
  connectionPut' c d = liftIO $ connectionPut c d
  connectionGetChunk'' c cont = liftIO $ connectionGetChunk' c cont

{-
getMessages :: IMAPConnection -> [Int] -> IO (Either Error [EmailMessage])
getMessages conn uids = liftM (\m -> m >>= id) parsedMessages
  where uidsToQuery      = T.intercalate "," $ map (T.pack . show) uids
        messages         = (liftM . liftM) (filter isFetch)
                           (simpleFormat $ uidFetch conn uidsToQuery)
        parsedMessages   = (liftM . liftM) (mapM parseMessage) messages
-}

parseMessage :: UntaggedResult -> Either MPT.ErrorMessage EmailMessage
parseMessage (Fetch msg) = maybeToEither "no body found" (find (isBody) msg)
                 >>= \(Body b) -> MP.parseMessage b
