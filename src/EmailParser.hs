module EmailParser (parseMessage, headerParser, messageParser) where

import EmailParsers.Parsers.Message (messageParser)
import qualified Data.ByteString.Char8 as BSC
import Data.Attoparsec.ByteString

parseMessage :: BSC.ByteString -> IO (Either ErrorMessage EmailMessage)
parseMessage message = do
  return $ parseOnly messageParser message >>= id
