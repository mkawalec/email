module EmailParser.Parsers.Header where

import EmailParser.Types
import EmailParser.Utils

import Data.Word8
import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as AP
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import qualified Data.ByteString.Char8 as BSC

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
