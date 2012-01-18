{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module HollaBack.Types (Payload(..),
                        EmailAddress(..),
                        ParseError(..)) where

import qualified Control.Exception as E
import Control.Applicative ((<$>),
                            (<*>))
import Data.Aeson (decode',
                   encode,
                   FromJSON(..),
                   ToJSON(..),
                   Value(..),
                   object,
                   (.=),
                   (.:))
import Data.Aeson.Types (typeMismatch)
import Data.Attoparsec.ByteString (parseOnly)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toChunks,
                             fromChunks)
import qualified Data.ByteString as BS
import Database.Redis.ByteStringClass

data Payload = Payload { from          :: EmailAddress,
                         to            :: EmailAddress,
                         subject       :: Text,
                         body          :: Text,
                         offsetSeconds :: Int } deriving (Show, Eq)

type EmailAddress = Text

instance BS Payload where
  toBS      = deLazy . encode 
  fromBS bs = fromMaybe (E.throw $ ParseError "Failed to parse") parsed
    where throwLeft (Left err) = E.throw $ ParseError err
          parsed = decode' $ reLazy bs

instance ToJSON Payload where
  toJSON pl = object ["from"           .= from pl,
                      "body"           .= body pl,
                      "to"             .= to pl,
                      "subject"        .= subject pl,
                      "offset_seconds" .= offsetSeconds pl ]

instance FromJSON Payload where
  parseJSON (Object v) = Payload <$> v .: "from"
                                 <*> v .: "to"
                                 <*> v .: "subject"
                                 <*> v .: "body"
                                 <*> v .: "offset_seconds"
  parseJSON v          = typeMismatch "Payload" v

data ParseError = ParseError String deriving (Show, Eq, Typeable)

instance E.Exception ParseError

deLazy = BS.concat . toChunks

reLazy = fromChunks . (:[])
