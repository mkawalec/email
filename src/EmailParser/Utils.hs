module EmailParser.Utils where

import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as AP
import Data.Word8

import Data.Text (Text)
import qualified Data.Text as T

consumeTillEndLine :: Parser [Word8]
consumeTillEndLine = manyTill anyWord8 $ string "\r\n"

isWhitespace :: Word8 -> Bool
isWhitespace x = x == 9 || x == 32

isConsequentHeaderLine :: Parser [Word8]
isConsequentHeaderLine = satisfy isWhitespace *>
                         AP.takeWhile isWhitespace *>
                         consumeTillEndLine

commentRemover :: Text -> Text
commentRemover contents = T.strip withoutComment
  where splitAtComment = T.split (\c -> c == '(' || c == ')') contents
        withoutComment = if length splitAtComment > 1
                          then T.append (head splitAtComment) (last splitAtComment)
                          else head splitAtComment
