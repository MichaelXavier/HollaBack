{-# LANGUAGE OverloadedStrings #-}
module HollaBack.Scheduler.Storage (persistHollaBack,
                                   pollForHollaBacks,
                                   getIncomingMessage,
                                   dateTimeSpecFromEmail,
                                   mailbox) where

import Control.Applicative ((<$>),
                            (<*>),
                            (<*),
                            pure)
import Control.Monad (when,
                      liftM)
import Control.Monad.Loops (unfoldM_)
import Data.Attoparsec.Text (parseOnly)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack,
                              unpack,
                              readInteger)
import Data.ByteString.UTF8 (fromString)
import Data.Maybe (fromJust,
                   fromMaybe,
                   maybeToList,
                   listToMaybe)
import Data.Text (breakOn, Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (getCurrentTime)
import Database.Redis.Redis (Redis,
                             Reply(..),
                             Interval(..),
                             blpop,
                             del,
                             llen,
                             lpop,
                             rpush,
                             zadd,
                             zrem,
                             zrangebyscore)
import Database.Redis.ByteStringClass

import HollaBack.Date.Types
import HollaBack.Date.Parser (dateTimeSpec)
import HollaBack.Date.Conversion (decideTime,
                                 timestamp)
import HollaBack.Scheduler.Types

--TODO: recipient?
persistHollaBack :: Redis -> Payload -> DateTimeSpec -> IO ()
persistHollaBack redis payload dts = do
  ts <- timestamp <$> decideTime dts
  -- add payload to the timestamp queue
  rpush redis (timestampKey ts) payload
  -- Add the timestamp with the timestamp value as the weight to the sorted set of keys
  zadd redis scheduleKey (fromIntegral ts) (toBS ts)
  return ()

dateTimeSpecFromEmail :: EmailAddress -> Either String DateTimeSpec
dateTimeSpecFromEmail ea = parseOnly dateTimeSpec $ mailbox ea

mailbox :: EmailAddress -> Text
mailbox = fst . breakOn "@"

pollForHollaBacks :: Redis -> (Payload -> IO ()) -> IO ()
pollForHollaBacks redis f = unfoldM_ $ poll =<< now
  where poll stopTs = do maybeTs <- nextTimestamp redis stopTs
                         whenJust maybeTs $ \ts -> do 
                           unfoldM_ $ do maybePayload <- nextHollaBackForTimestamp redis ts
                                         whenJust maybePayload f
                                         return maybePayload
                         return maybeTs

-- Note: blocks until it receives a message
getIncomingMessage :: Redis -> IO Payload
getIncomingMessage redis = do
  Just (_, rawMessage) <- blpop redis [mailKey] timeout
  return rawMessage
  where timeout = 0

---- Helpers

whenJust :: Maybe a -> (a -> IO ()) -> IO ()
whenJust (Just x) f = f x
whenJust Nothing _ = return ()

type TimeStamp = Integer

instance BS Integer where
  toBS   = fromString . show
  fromBS = fst . fromJust . readInteger

nextTimestamp :: Redis -> TimeStamp -> IO (Maybe TimeStamp)
nextTimestamp redis stopTs = do
  RMulti timestamps <- zrangebyscore redis scheduleKey interval lims withScores
  return $ unwrap <$> (listToMaybe $ fromMaybe [] timestamps)
  where interval                 = LeftOpen 0 dblTs -- Does not seem to let us use +/- infinity :(
        lims                     = Just (0, 1)
        withScores               = False
        unwrap (RBulk (Just bs)) = read . unpack $ bs
        dblTs                    = fromIntegral stopTs

remTimestampIfEmpty :: Redis -> TimeStamp -> IO ()
remTimestampIfEmpty redis ts = do
  remaining <- unwrapLen <$> llen redis tsk
  when (remaining == 0) remove
  where tsk                = timestampKey ts
        remove             = del redis tsk >> zrem redis scheduleKey ts >> return ()
        unwrapLen (RInt i) = fromIntegral i

nextHollaBackForTimestamp :: Redis -> TimeStamp -> IO (Maybe Payload)
nextHollaBackForTimestamp redis ts = popPayload <* remTimestampIfEmpty redis ts
  where popPayload         = unwrap <$> lpop redis tsk
        tsk                = timestampKey ts
        unwrap (RBulk mts) = mts
        --TODO: not exhaustive?

timestampKey :: TimeStamp -> ByteString
timestampKey ts = scopeKey ["timestamps", toBS ts]

mailKey :: ByteString
mailKey = scopeKey ["messages"]

scopeKey :: [ByteString] -> ByteString
scopeKey xs = BS.intercalate scopeSeparator $ keyPrefix:xs

scheduleKey :: ByteString
scheduleKey = scopeKey ["schedule"]

keyPrefix :: ByteString
keyPrefix = "followup"

scopeSeparator :: ByteString
scopeSeparator = ":"

now :: IO TimeStamp
now = timestamp <$> getCurrentTime
