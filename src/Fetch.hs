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

getMessageIds :: IMAPConnection -> IO (Either Error [Int])
getMessageIds conn = unpack allIds
  where unpackSearch = \(Search nums) -> nums
        allIds = simpleFormat $ uidSearch conn "ALL"
        unpack = (liftM . liftM) (concat . map unpackSearch . filter isSearch)

getMessages :: IMAPConnection -> [Int] -> IO (Either Error [EmailMessage])
getMessages conn uids = liftM (\m -> m >>= id) parsedMessages
  where uidsToQuery      = T.intercalate "," $ map (T.pack . show) uids
        messages         = (liftM . liftM) (filter isFetch)
                           (simpleFormat $ uidFetch conn uidsToQuery)
        parsedMessages   = (liftM . liftM) (mapM parseMessage) messages

parseMessage :: UntaggedResult -> Either MPT.ErrorMessage EmailMessage
parseMessage (Fetch msg) = maybeToEither "no body found" (find (isBody) msg)
                 >>= \(Body b) -> MP.parseMessage b
