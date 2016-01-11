module EmailParser.Parsers.MIME where

import qualified Data.ByteString.Char8 as BSC

import Types
import EmailParser.Parsers.Multipart (parseMultipart)
import EmailParser.Parsers.Header (headerParser)
import EmailParser.Decoders.BodyDecoder (decodeBody, decodeTextBody)
import EmailParser.Parsers.Utils (discoverAttachment, isBroken)
import EmailParser.Types
import EmailParser.Utils

import Data.Either.Utils (maybeToEither)
import Data.Attoparsec.ByteString
import Data.List (find)
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T

import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Either (rights, isRight)

import Codec.MIME.Parse (parseMIMEType)
import Codec.MIME.Type

import qualified Debug.Trace as DT
import Control.Monad (foldM)

-- |Parses a MIME message part. Needs headers from the actual message
-- in case the MIME block misses some encoding blocks
mimeParser :: [Header] -> Parser EmailBody
mimeParser bodyHeaders = do
  headers <- manyTill' headerParser $ string "\r\n"
  body <- takeByteString

  let isAttachment = discoverAttachment headers
  if isJust isAttachment
    then do
      let filename = fromJust isAttachment
      let decodedBody = decodeBody headers body
      return $ Attachment filename decodedBody
    else return $! MIMEBody headers $ decodeTextBody (headers ++ bodyHeaders) body

-- |Parse a set of parts.
multipartParser :: [Header] -> [BSC.ByteString] -> Either ErrorMessage [EmailBody]
multipartParser bodyHeaders parts = do
  let parsed = map (\x -> parseOnly (mimeParser bodyHeaders) x) parts
  foldM isBroken [] parsed

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right a) = Just a
eitherToMaybe (Left e) = Nothing

-- |Parse a mime encoded body.
parseMIME :: [Header] -> BSC.ByteString -> Either ErrorMessage [EmailBody]
parseMIME headers body = case isRight msgType of
  True -> case mimeType . head . rights $ [msgType] of
    Multipart _ -> multiParsed >>= multipartParser headers
    Text _ -> Right decodedBody
    _ -> Left "mimetype not supported"
  False -> Right decodedBody
  where msgType = findHeader "Content-Type" headers >>=
          Right . parseMIMEType . headerContents >>=
          maybeToEither "Couldn't parse message type"
        multiParsed = msgType >>=
          \x -> maybeToEither "asdads" $ find (\p -> paramName p == "boundary") (mimeParams x) >>=
          return . encodeUtf8 . paramValue >>=
          \b -> eitherToMaybe $ parseOnly (parseMultipart b) body
        decodedBody = [TextBody $ decodeTextBody headers body]
