module EmailParser.Parsers.Utils where

import EmailParser.Types
import Types
import EmailParser.Utils

import qualified Data.Text as T
import Data.List


-- |Add the current body to the list if it succeeds, if
-- it doesn't return the error. Looks like it could be
-- handled by an instance of some typeclass
isBroken :: [EmailBody] -> Either ErrorMessage EmailBody -> Either ErrorMessage [EmailBody]
isBroken bodies current = case current of
  Right val -> Right (val:bodies)
  Left err -> Left err

-- |Check if the given headers represent an attachment
discoverAttachment :: [Header] -> Maybe T.Text
discoverAttachment headers = hdr >>= findAttachmentName . headerContents
  where hdr = find (\x -> (T.toLower . headerName $ x) == "content-disposition") headers

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

-- |Decide if the header contains a valid MIME info
isMIME :: Header -> Bool
isMIME header = isNameValid && isVersionValid
  where isNameValid    = headerName header == "MIME-Version"
        version        = commentRemover $ headerContents header
        isVersionValid = version == "1.0"
