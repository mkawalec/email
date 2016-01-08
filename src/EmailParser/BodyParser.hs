module EmailParser.BodyParser where

import qualified Data.ByteString.Char8 as BS

import qualified Codec.Binary.Base64 as B64
import Codec.MIME.Parse (parseMIMEType)
import Codec.MIME.Type

import Data.Either.Combinators (mapLeft)
import Data.Either.Utils (maybeToEither)
import Data.Either (isRight, rights, lefts)
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
import EmailParser.Decoders (qp_dec)


transferDecode :: BS.ByteString -> Text -> Either (BS.ByteString, BS.ByteString) BS.ByteString
transferDecode body encoding = case T.toLower encoding of
  "quoted-printable" -> qp_dec body
  "base64" -> B64.decode body
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

parseTextBody :: [Header] -> BS.ByteString -> Either ErrorMessage Text
parseTextBody headers body =
  if isRight contentType
    then if isRight decodedBody
          then charset >>= return . toText (head . rights $ [decodedBody])
          else charset >>= return . toText body
    else Right $ toText body "utf-8"
  where decodedBody = findHeader "Content-Transfer-Encoding" headers >>=
          return . headerContents >>=
          \h -> mapLeft (\_ -> "Decoding error") (transferDecode body h)
        noMIME = "No mimetype declaration could be found"
        noCharset = "No charset could be found"
        contentType = findHeader "Content-Type" headers
        charset = contentType >>=
          \h -> maybeToEither noMIME (parseMIMEType $ headerContents h) >>=
          \m -> maybeToEither noCharset $ find (\x -> (paramName x) == "charset") (mimeParams m) >>=
          return . paramValue
