module EmailParser.Parsers.Message where

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
import EmailParser.MIME (isValidMIME, parseMIME)
import EmailParser.BodyParser (decodeTextBody)
import EmailParser.MIME.Multipart
import EmailParser.HeaderParser (headerParser)

-- |Parses a single message
messageParser :: Parser (Either ErrorMessage EmailMessage)
messageParser = do
  headers <- manyTill' headerParser $ string "\r\n"
  body <- takeByteString

  -- Parse MIME if the message is in a MIME format
  let parsedBody = if isJust $ find isValidMIME headers
                    then parseMIME headers body
                    else decodeTextBody headers body >>= \x -> return $ [TextBody x]
  return $! parsedBody >>= return . EmailMessage headers

-- |Parses a header
headerParser :: Parser Header
headerParser = do
  headerName <- AP.takeWhile (/= _colon)
  word8 _colon
  AP.takeWhile isWhitespace

  headerLine <- consumeTillEndLine
  moreLines <- many' isConsequentHeaderLine
  let headerBody = cleanupLines $ [headerLine] ++ moreLines
  return $ Header (decodeUtf8 headerName) (decodeUtf8 headerBody)

-- |Concatenate lines insterting whitespace between them.
-- The whitespace needs to be inserted as these lines
-- come from parser that eats up the whitespace
cleanupLines :: [BSC.ByteString] -> BSC.ByteString
cleanupLines ls = BSC.intercalate " " $ map BSC.init ls
