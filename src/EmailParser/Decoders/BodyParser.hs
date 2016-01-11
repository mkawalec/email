module EmailParser.Decoders.BodyParser where

import qualified Data.ByteString.Char8 as BS

import qualified Codec.Binary.Base64 as B64
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


-- |Remove transfer encoding from a string of bytes
transferDecode :: BS.ByteString -> Text -> Either (BS.ByteString, BS.ByteString) BS.ByteString
transferDecode body encoding = case T.toLower encoding of
  "quoted-printable" -> qp_dec body
  "base64" -> decode_b64 body
  _ -> Right body

-- |Transform a string of bytes with a given encoding
-- into a UTF-8 string of bytes
encodingToUtf :: BS.ByteString -> Text -> Text
encodingToUtf body encoding = case T.toLower encoding of
  "utf-8" -> decodeUtf8 body
  _ -> ICU.toUnicode converter body
    where converter = unsafePerformIO $ ICU.open (T.unpack encoding) (Just True)

-- |Reverse content transfer encoding applied to the body.
decodeBody :: [Header] -> BS.ByteString -> BS.ByteString
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
decodeTextBody :: [Header] -> BS.ByteString -> Text
decodeTextBody headers body =
  if isRight charset
    then encodingToUtf decodedBody (head . rights [charset])
    else decodeUtf8 decodedBody
  where decodedBody = decodeBody headers body
        charset = findHeader "Content-Type" headers >>=
          \h -> maybeToEither "" (parseMIMEType $ headerContents h) >>=
          \m -> maybeToEither "" $ find (\x -> (paramName x) == "charset") (mimeParams m) >>=
          return . paramValue

-- |Given a header name, it will try to locate it in
-- a list of headers, fail if it's not there
findHeader :: Text -> [Header] -> Either ErrorMessage Header
findHeader hdr headers = maybeToEither notFound header
  where notFound    = "Cound not find header '" ++ (show hdr) ++ "'"
        eigenHeader = T.toLower hdr
        header      = find (\x -> (T.toLower $ headerName x) == eigenHeader) headers
