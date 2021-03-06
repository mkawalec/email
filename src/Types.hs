module Types where

import Data.Text
import GHC.Generics
import qualified Data.Yaml as YAML
import Data.Yaml ((.:))
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import Control.Applicative
import qualified Network.IMAP.Types as IMAP
import qualified Network.Mail.Parse.Types as MP
import qualified Data.HashMap.Strict as HM

import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField
import Data.Text.Encoding (encodeUtf8)

type Error = Text
type UID = Int
type Metadata = [IMAP.UntaggedResult]

data AccountConfig = AccountConfig {
  accountName :: Text,
  accountLogin :: Text,
  accountPassword :: Text,
  accountServer :: Text,
  accountPort :: Integer
} deriving (Show, Eq, Ord, Generic)

data Config = Config {
  accounts :: [AccountConfig],
  dbHost :: Text
} deriving (Show, Eq, Ord, Generic)


instance YAML.FromJSON Config
instance YAML.FromJSON AccountConfig where
  parseJSON (YAML.Object v) = AccountConfig <$>
                         v .: "name" <*>
                         v .: "login" <*>
                         v .: "password" <*>
                         v .: "server" <*>
                         v .: "port"
  parseJSON _ = error "Wrong input format, needs an object"

data RelationType = TO | CC | BCC
  deriving (Eq, Show)

instance ToField RelationType where
  toField TO = Escape . encodeUtf8 $ "To"
  toField CC = Escape . encodeUtf8 $ "CC"
  toField BCC = Escape . encodeUtf8 $ "BCC"

instance FromField RelationType where
  fromField f bs
    | bs == Nothing    = returnError UnexpectedNull f ""
    | bs == Just "To"  = pure TO
    | bs == Just "CC"  = pure CC
    | bs == Just "BCC" = pure BCC
    | otherwise        = returnError ConversionFailed f ""

instance Aeson.FromJSON MP.EmailMessage
instance Aeson.FromJSON MP.Header
instance Aeson.FromJSON MP.EmailBody where
  parseJSON (Aeson.String s) = return $ MP.TextBody s
  parseJSON obj@(Aeson.Object v) = if HM.member "storageFilename" v
    then do
      MP.Attachment
      <$> v .: "headers"
      <*> v .: "name"
      <*> (return Nothing)
      <*> v .: "storageFilename"
    else MP.MessageBody <$> Aeson.parseJSON obj
  parseJSON _ = error "don't know how to decode that"
instance Aeson.FromJSON MP.EmailAddress

instance Aeson.ToJSON MP.EmailMessage
instance Aeson.ToJSON MP.Header
instance Aeson.ToJSON MP.EmailAddress
instance Aeson.ToJSON MP.EmailBody where
  toJSON (MP.MessageBody m) = Aeson.toJSON m
  toJSON (MP.TextBody text) = Aeson.String text
  toJSON (MP.Attachment hdrs name _ filename) = Aeson.object [
    "headers" .= Aeson.toJSON hdrs,
    "name" .= Aeson.String name,
    "storageFilename" .= Aeson.toJSON filename]
