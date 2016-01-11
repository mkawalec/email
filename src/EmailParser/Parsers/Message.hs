module EmailParser.Parsers.Message (messageParser) where

import Data.Word8
import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as AP
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import Debug.Trace as DT

import Data.List (find)
import Data.Maybe

import Types
import EmailParser.Types
import EmailParser.Utils
import EmailParser.Parsers.MIME (parseMIME)
import EmailParser.Parsers.Utils (isMIME)
import EmailParser.Decoders.BodyDecoder (decodeTextBody)
import EmailParser.Parsers.Multipart (parseMultipart)
import EmailParser.Parsers.Header (headerParser)

-- |Parses a single message
messageParser :: Parser (Either ErrorMessage EmailMessage)
messageParser = do
  headers <- manyTill' headerParser $ string "\r\n"
  body <- takeByteString

  -- Parse MIME if the message is in a MIME format
  let parsedBody = if isJust $ find isMIME headers
      then parseMIME headers body
      else Right $ [TextBody $ decodeTextBody headers body]
  return $! parsedBody >>= return . EmailMessage headers
