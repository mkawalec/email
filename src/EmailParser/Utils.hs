module EmailParser.Utils where

import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Char8 as BS
import Debug.Trace as DT
import Data.Word8

import Data.Text (Text)
import qualified Data.Text as T

hadCRLF :: Word8 -> Word8 -> Maybe Word8
hadCRLF prev current =
  if current == _lf
    then if prev == _cr then DT.trace "breaking" $ Nothing else Just current
    else Just current

consumeTillEndLine :: Parser BS.ByteString
consumeTillEndLine = scan 0 hadCRLF <* satisfy (== _lf)

isWhitespace :: Word8 -> Bool
isWhitespace x = x == 9 || x == 32

isConsequentHeaderLine :: Parser BS.ByteString
isConsequentHeaderLine = satisfy isWhitespace *>
                         AP.takeWhile isWhitespace *>
                         consumeTillEndLine

commentRemover :: Text -> Text
commentRemover contents = T.strip withoutComment
  where splitAtComment = T.split (\c -> c == '(' || c == ')') contents
        withoutComment = if length splitAtComment > 1
                          then T.append (head splitAtComment) (last splitAtComment)
                          else head splitAtComment
