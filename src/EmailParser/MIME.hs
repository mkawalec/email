module EmailParser.MIME where

import qualified Data.ByteString.Char8 as BS

import EmailParser.MIME.Multipart (parseMultipart)
import Data.Either.Utils (maybeToEither)
import Data.Attoparsec.ByteString
import Data.List (find)
import Data.Maybe (fromJust)
import qualified Data.Text as T

import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import Codec.MIME.Parse (parseMIMEType)
import Codec.MIME.Type

import EmailParser.Types
import EmailParser.Utils
import EmailParser.BodyParser (parseTextBody)
import Types

isValidMIME :: Header -> Bool
isValidMIME header = isNameValid && isVersionValid
  where isNameValid    = headerName header == "MIME-Version"
        version        = commentRemover $ headerContents header
        isVersionValid = version == "1.0"

defaultMIMEBody = MIMEBody [] ""

parseMIME :: [Header] -> BS.ByteString -> Either ErrorMessage [EmailBody]
parseMIME headers body = case mimeType . fromJust $ msgType of
    Multipart _ -> multiParsed >>= return . map (\x -> defaultMIMEBody {mimeBody=decodeUtf8 x})
    Text _ -> parseTextBody headers body >>= \x -> return [TextBody x]
    _ -> Left "mimetype not supported"
  where typeHeader  = find (\h -> headerName h == "Content-Type") headers
        msgType     = typeHeader >>= parseMIMEType . headerContents
        boundaryP   = find (\p -> paramName p == "boundary") $ mimeParams . fromJust $ msgType
        boundary    = (maybeToEither "No boundary parameter supplied") boundaryP >>=
                        return . encodeUtf8 . paramValue
        multiParsed = boundary >>= \b -> parseOnly (parseMultipart b) body
