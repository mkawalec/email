module EmailParser (parseMessage, headerParser, messageParser) where

import Data.Word8
import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as AP
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString as B

import Data.List (find)
import Data.Maybe

import Types
import EmailParser.Types
import EmailParser.Utils
import EmailParser.MIME (isValidMIME, parseMIME)
import EmailParser.BodyParser (parseTextBody)
import EmailParser.MIME.Multipart

headerParser :: Parser Header
headerParser = do
  headerName <- AP.takeWhile (/= _colon)
  word8 _colon
  AP.takeWhile isWhitespace

  headerLine <- consumeTillEndLine
  moreLines <- many' isConsequentHeaderLine
  let headerBody = headerLine ++ concat moreLines
  return $ Header (BS.unpack headerName) (decodeUtf8 . B.pack $ headerBody)

messageParser :: Parser (Either ErrorMessage EmailMessage)
messageParser = do
  headers <- manyTill' headerParser $ string "\r\n"
  body <- takeByteString

  -- Parse MIME if the message is in a MIME format
  let parsedBody = if isJust $ find isValidMIME headers
                    then parseMIME headers body
                    else parseTextBody headers body >>= \x -> return $ [TextBody x]
  return $! parsedBody >>= return . EmailMessage headers

parseMessage :: BS.ByteString -> IO (Either ErrorMessage EmailMessage)
parseMessage message = do
  return $ parseOnly messageParser message >>= id
