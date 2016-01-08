module EmailParser.Types where

import Data.Text
import qualified Data.ByteString.Char8 as BS

data EmailMessage = EmailMessage {
  emailHeaders :: [Header],
  emailBodies :: [EmailBody]
} deriving (Show)

data Header = Header {
  headerName :: String,
  headerContents :: Text
} deriving (Show)

data EmailBody = MIMEBody { mimeHeaders :: [Header], mimeBody :: Text} |
                 TextBody Text |
                 Attachment {attachementName :: Text, attachementBody :: BS.ByteString}
                 deriving (Show)
