module EmailParser.Types where

import Data.Text
import qualified Data.ByteString.Char8 as BS

data EmailMessage = EmailMessage {
  emailHeaders :: [Header],
  emailBodies :: [EmailBody]
} deriving (Show)

data Header = Header {
  headerName :: Text,
  headerContents :: Text
} deriving (Show)

-- |An email body contains the contents of an email part
-- up until the boundary marker.
data EmailBody
  -- |Body of a MIME message part. Contains headers
  = MIMEBody { mimeHeaders :: ![Header], mimeBody :: !Text}
  -- |If the message contained no MIME information, it's probably
  -- just some text. Best guess decoding into UTF-8 is applied
  | TextBody !Text
  -- |Attachement is a part of a MIME message, but a rather special
  -- one. It's decoded from whatever the transfer encoding was applied
  -- and left as a raw sollection of bytes for your enjoyment
  | Attachment {attachmentName :: !Text, attachmentBody :: !BS.ByteString}
    deriving (Show)
