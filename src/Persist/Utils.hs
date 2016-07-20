module Persist.Utils where

import qualified Network.Mail.Parse.Types as MPT
import qualified Data.Text as T

-- |Transforms an email address into a DB-saveable format
saveableAddress :: MPT.EmailAddress -> (T.Text, Maybe T.Text)
saveableAddress (MPT.EmailAddress addr label) = (addr, label)

-- |Get all email addresses that exist in message metadata
getSaveableAddresses :: MPT.EmailMessage -> [MPT.EmailAddress]
getSaveableAddresses msg = concatMap extractHeader (MPT.emailHeaders msg)

extractHeader :: MPT.Header -> [MPT.EmailAddress]
extractHeader header = case header of
  MPT.From addr -> [addr]
  MPT.ReplyTo addr -> [addr]
  MPT.To addrs -> addrs
  MPT.CC addrs -> addrs
  MPT.BCC addrs -> addrs
  _ -> []


-- ALL HAIL THE GLORIOUS BOILERPLATE! GLORY TO THE BOILERPLATE GODS!
isFrom (MPT.From _) = True
isFrom _ = False

isDate (MPT.Date _) = True
isDate _ = False

isReplyTo (MPT.ReplyTo _) = True
isReplyTo _ = False

isMessageId (MPT.MessageId _) = True
isMessageId _ = False

isInReplyTo (MPT.InReplyTo _) = True
isInReplyTo _ = False

isSubject (MPT.Subject _) = True
isSubject _ = False
