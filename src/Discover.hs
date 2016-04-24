module Discover where

import Network.IMAP
import Network.IMAP.Types
import Control.Applicative
import qualified Data.Text as T


getAllMailboxes :: IMAPConnection -> IO (Either T.Text [T.Text])
getAllMailboxes conn = do
    mboxes <- simpleFormat $ list conn "*"
    return $ getNames <$> mboxes
  where getNames = map inboxName . removeVirtualMBoxes

removeVirtualMBoxes :: [UntaggedResult] -> [UntaggedResult]
removeVirtualMBoxes = filter (not . hasChildren) . prefilter
  where prefilter = filter isListR
        hasChildren = elem (OtherNameAttr "HasChildren") . flags
