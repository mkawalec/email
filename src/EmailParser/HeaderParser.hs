module EmailParser.HeaderParser where

import qualified Data.Attoparsec.ByteString as AP
import Data.Attoparsec.ByteString
import qualified Data.ByteString.Char8 as BS
import Data.Word8
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import EmailParser.Types
import EmailParser.Utils

cleanupLines :: [BS.ByteString] -> BS.ByteString
cleanupLines ls = BS.intercalate " " $ map BS.init ls

headerParser :: Parser Header
headerParser = do
  headerName <- AP.takeWhile (/= _colon)
  word8 _colon
  AP.takeWhile isWhitespace

  headerLine <- consumeTillEndLine
  moreLines <- many' isConsequentHeaderLine
  let headerBody = cleanupLines $ [headerLine] ++ moreLines
  return $ Header (BS.unpack headerName) (decodeUtf8 headerBody)
