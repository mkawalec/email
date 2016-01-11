module EmailParser (parseMessage) where

import Types
import EmailParser.Types
import EmailParser.Parsers.Message (messageParser)

import qualified Data.ByteString.Char8 as BSC
import Data.Attoparsec.ByteString

-- |Parses a single message of any mimetype
parseMessage :: BSC.ByteString -> IO (Either ErrorMessage EmailMessage)
parseMessage message =
  return $ parseOnly messageParser message >>= id
