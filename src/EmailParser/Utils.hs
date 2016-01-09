module EmailParser.Utils where

import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Char8 as BS
import Debug.Trace as DT
import Data.Word8

import Data.Text (Text)
import qualified Data.Text as T

-- |If the previous character was a carriage return and the current
-- is a line feed, stop parsing
hadCRLF :: Word8 -> Word8 -> Maybe Word8
hadCRLF prev current =
  if current == _lf
    then if prev == _cr then Nothing else Just current
    else Just current

-- |Consumes a line until CRLF is hit
consumeTillEndLine :: Parser BS.ByteString
consumeTillEndLine = scan 0 hadCRLF <* satisfy (== _lf)

-- |Can a given character be regarded as a whitespace?
isWhitespace :: Word8 -> Bool
isWhitespace x = x == 9 || x == 32

-- |If the next line is a part of a previous header, parse it.
-- Fail otherwise
isConsequentHeaderLine :: Parser BS.ByteString
isConsequentHeaderLine = satisfy isWhitespace *>
                         AP.takeWhile isWhitespace *>
                         consumeTillEndLine

-- |Remove a MIME header comment and return a header without the comment
commentRemover :: Text -> Text
commentRemover contents = T.strip withoutComment
  where splitAtComment = T.split (\c -> c == '(' || c == ')') contents
        withoutComment = if length splitAtComment > 1
                          then T.append (head splitAtComment) (last splitAtComment)
                          else head splitAtComment
