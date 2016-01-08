module EmailParser.BodyParser where

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
import EmailParser.Decoders (qp_dec, decode_b64)


transferDecode :: BS.ByteString -> Text -> Either (BS.ByteString, BS.ByteString) BS.ByteString
transferDecode body encoding = case T.toLower encoding of
  "quoted-printable" -> qp_dec body
  "base64" -> decode_b64 body
  _ -> Right body

toText :: BS.ByteString -> Text -> Text
toText body encoding = case T.toLower encoding of
  "utf-8" -> decodeUtf8 body
  _ -> ICU.toUnicode converter body
    where converter = unsafePerformIO $ ICU.open (T.unpack encoding) (Just True)

findHeader :: String -> [Header] -> Either ErrorMessage Header
findHeader hdr headers = maybeToEither notFound header
  where notFound = "Cound not find header '" ++ hdr ++ "'"
        header   = find (\x -> (headerName x) == hdr) headers

decodeBody :: [Header] -> BS.ByteString -> BS.ByteString
decodeBody headers body =
  if isRight decodedBody
    then head . rights $ [decodedBody]
    else body
  where decodedBody = findHeader "Content-Transfer-Encoding" headers >>=
          return . headerContents >>=
          \h -> mapLeft (\_ -> "Decoding error") (transferDecode body h)

parseTextBody :: [Header] -> BS.ByteString -> Either ErrorMessage Text
parseTextBody headers body =
  if isRight charset
    then charset >>= return . toText decodedBody
    else Right $ decodeUtf8 decodedBody
  where decodedBody = decodeBody headers body
        noMIME = "No mimetype declaration could be found"
        noCharset = "No charset could be found"
        charset = findHeader "Content-Type" headers >>=
          \h -> maybeToEither noMIME (parseMIMEType $ headerContents h) >>=
          \m -> maybeToEither noCharset $ find (\x -> (paramName x) == "charset") (mimeParams m) >>=
          return . paramValue
