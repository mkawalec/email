module EmailParser.MIME where

import qualified Data.ByteString.Char8 as BS

import EmailParser.MIME.Multipart (parseMultipart)
import EmailParser.HeaderParser (headerParser)
import Data.Either.Utils (maybeToEither)
import Data.Attoparsec.ByteString
import Data.List (find)
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T

import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Either (rights)

import Codec.MIME.Parse (parseMIMEType)
import Codec.MIME.Type

import EmailParser.Types
import EmailParser.Utils
import EmailParser.BodyParser (parseTextBody)
import Types
import qualified Debug.Trace as DT
import Control.Monad (foldM)

isValidMIME :: Header -> Bool
isValidMIME header = isNameValid && isVersionValid
  where isNameValid    = headerName header == "MIME-Version"
        version        = commentRemover $ headerContents header
        isVersionValid = version == "1.0"

defaultMIMEBody = MIMEBody [] ""

mimeParser :: [Header] -> Parser (Either ErrorMessage EmailBody)
mimeParser bodyHeaders = do
  headers <- manyTill' headerParser $ string "\r\n"
  body <- takeByteString

  return $ DT.traceShow headers $ (parseTextBody (headers ++ bodyHeaders) body >>= return . MIMEBody headers)

isBroken :: [EmailBody] -> Either ErrorMessage EmailBody -> Either ErrorMessage [EmailBody]
isBroken bodies current = case current of
  Right val -> Right (val:bodies)
  Left err -> Left err

multipartParser :: [Header] -> [BS.ByteString] -> Either ErrorMessage [EmailBody]
multipartParser bodyHeaders parts = do
  let parsed = map (\x -> parseOnly (mimeParser bodyHeaders) x >>= id) parts
  foldM isBroken [] parsed

parseMIME :: [Header] -> BS.ByteString -> Either ErrorMessage [EmailBody]
parseMIME headers body = case isJust msgType of
  True -> case mimeType . fromJust $ msgType of
    Multipart _ -> multiParsed >>= multipartParser headers
    Text _ -> parseTextBody headers body >>= \x -> return [TextBody x]
    _ -> Left "mimetype not supported"
  False -> parseTextBody headers body >>= \x -> return [TextBody x]
  where typeHeader  = find (\h -> headerName h == "Content-Type") headers
        msgType     = typeHeader >>= parseMIMEType . headerContents
        boundaryP   = find (\p -> paramName p == "boundary") $ mimeParams . fromJust $ msgType
        boundary    = (maybeToEither "No boundary parameter supplied") boundaryP >>=
                        return . encodeUtf8 . paramValue
        multiParsed = boundary >>= \b -> parseOnly (parseMultipart b) body
