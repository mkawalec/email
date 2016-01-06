module EmailParser.MIME.Multipart (parseMultipart) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString as B
import Data.Word8
import Data.Attoparsec.ByteString

import EmailParser.Utils

isBoundaryMatched :: BS.ByteString -> Int -> Word8 -> Maybe Int
isBoundaryMatched boundary matchIdx char =
  if char == (B.index boundary matchIdx)
    then if matchIdx == boundaryLength - 1
          then Nothing
          else Just $ matchIdx + 1
    else Just 0
  where boundaryLength = B.length boundary

trimPayload :: BS.ByteString -> BS.ByteString -> BS.ByteString
trimPayload boundary payload = BS.take trimLength payload
  where payloadLength  = BS.length payload
        boundaryLength = BS.length boundary
        trimLength     = payloadLength - boundaryLength + 1

parseMultipart :: BS.ByteString -> Parser [BS.ByteString]
parseMultipart boundary =
  do
    (manyTill' anyWord8 $ string completeBoundary) <* consumeTillEndLine
    payloads <- many' (scan 0 (isBoundaryMatched completeBoundary) <* consumeTillEndLine)
    return $ map (trimPayload completeBoundary) payloads
  where completeBoundary = BS.append "\r\n--" boundary
        endBoundary      = BS.append completeBoundary "--\r\n"
