module EmailParser.MIME where

import qualified Data.ByteString.Char8 as BS

import EmailParser.MIME.Multipart (parseMultipart)
import EmailParser.HeaderParser (headerParser)
import EmailParser.BodyParser (decodeBody)
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

-- |Decide if the header contains a valid MIME info
isValidMIME :: Header -> Bool
isValidMIME header = isNameValid && isVersionValid
  where isNameValid    = headerName header == "MIME-Version"
        version        = commentRemover $ headerContents header
        isVersionValid = version == "1.0"

-- |When provided with a content-disposition header,
-- checks if it represents an attachment. If it does it returns
-- it's name, otherwise Nothing
findAttachmentName :: T.Text -> Maybe T.Text
findAttachmentName header =
  if (T.toLower . T.strip $ split !! 0) == "attachment"
    then if length split == 0
      then Just ""
      else filenameParam >>= return . T.strip . T.dropAround (== '"') . (!! 1)
    else Nothing
  where split = T.splitOn ";" header
        paramSplit = map (T.splitOn "=") (tail split)
        filenameParam = find (\x -> T.strip (x !! 0) == "filename") paramSplit

-- |Check if the given headers represent an attachment
discoverAttachment :: [Header] -> Maybe T.Text
discoverAttachment headers = hdr >>= findAttachmentName . headerContents
  where hdr = find (\x -> (T.toLower . headerName $ x) == "content-disposition") headers

-- |Parses a MIME message part. Needs headers from the actual message
-- in case the MIME block misses some encoding blocks
mimeParser :: [Header] -> Parser (Either ErrorMessage EmailBody)
mimeParser bodyHeaders = do
  headers <- manyTill' headerParser $ string "\r\n"
  body <- takeByteString

  let isAttachment = discoverAttachment headers
  if isJust isAttachment
    then do
      let filename = fromJust isAttachment
      let decodedBody = decodeBody headers body
      return . Right $ Attachment filename decodedBody
    else return $! parseTextBody (headers ++ bodyHeaders) body >>= return . MIMEBody headers

-- |Add the current body to the list if it succeeds, if
-- it doesn't return the error. Looks like it could be
-- handled by an instance of some typeclass
isBroken :: [EmailBody] -> Either ErrorMessage EmailBody -> Either ErrorMessage [EmailBody]
isBroken bodies current = case current of
  Right val -> Right (val:bodies)
  Left err -> Left err

-- |Parse a set of parts. Only returns results if the parse of all of them succeded
multipartParser :: [Header] -> [BS.ByteString] -> Either ErrorMessage [EmailBody]
multipartParser bodyHeaders parts = do
  let parsed = map (\x -> parseOnly (mimeParser bodyHeaders) x >>= id) parts
  foldM isBroken [] parsed

-- |Parse a mime encoded body.
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
