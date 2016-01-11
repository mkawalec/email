module EmailParser.Decoders.BodyDecoder where

import qualified Data.ByteString.Char8 as BSC

import Codec.MIME.Parse (parseMIMEType)
import Codec.MIME.Type

import Data.Either.Combinators (mapLeft)
import Data.Either.Utils (maybeToEither)
import Data.Either (isRight, isLeft, rights, lefts)
import Data.Maybe (fromJust, isJust)

import Data.List (find)

import Data.Text (Text, splitOn)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Debug.Trace as DT
import qualified Data.Text.ICU.Convert as ICU
import System.IO.Unsafe (unsafePerformIO)

import Types
import EmailParser.Types
import EmailParser.Decoders.FormatDecoders (qp_dec, decode_b64)
import EmailParser.Utils (findHeader)


-- |Remove transfer encoding from a string of bytes
transferDecode :: BSC.ByteString -> Text -> Either (BSC.ByteString, BSC.ByteString) BSC.ByteString
transferDecode body encoding = case T.toLower encoding of
  "quoted-printable" -> qp_dec body
  "base64" -> decode_b64 body
  _ -> Right body

-- |Transform a string of bytes with a given encoding
-- into a UTF-8 string of bytes
encodingToUtf :: BSC.ByteString -> Text -> Text
encodingToUtf body encoding = case T.toLower encoding of
  "utf-8" -> decodeUtf8 body
  _ -> ICU.toUnicode converter body
    where converter = unsafePerformIO $ ICU.open (T.unpack encoding) (Just True)

-- |Reverse content transfer encoding applied to the body.
decodeBody :: [Header] -> BSC.ByteString -> BSC.ByteString
decodeBody headers body =
  if isRight decodedBody
    then head . rights $ [decodedBody]
    else body
  where decodedBody = findHeader "Content-Transfer-Encoding" headers >>=
          return . headerContents >>=
          \h -> mapLeft (\_ -> "Decoding error") (transferDecode body h)

-- |Given a set of headers it tries to figure out
-- the transfer encoding and charset and normalizes
-- the contents into an UTF-8 encoded Text.
--
-- It will recover from errors, wherever possible
decodeTextBody :: [Header] -> BSC.ByteString -> Text
decodeTextBody headers body =
  if isRight charset
    then encodingToUtf decodedBody (head . rights $ [charset])
    else decodeUtf8 decodedBody
  where decodedBody = decodeBody headers body
        charset = findHeader "Content-Type" headers >>=
          \h -> maybeToEither "" (parseMIMEType $ headerContents h) >>=
          \m -> maybeToEither "" $ find (\x -> (paramName x) == "charset") (mimeParams m) >>=
          return . paramValue
