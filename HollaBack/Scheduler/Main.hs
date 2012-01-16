{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Spawn (spawn)
import           Control.Monad (forever)
import           Data.Attoparsec.Text (parseOnly)
import           Data.ByteString.Char8 (unpack, pack)
import qualified Data.ByteString as BS
import           Database.Redis.Redis
import qualified Data.Text.IO as TIO
import           System.IO (hPutStrLn,
                            stderr)

import           HollaBack.Date.Parser (dateTimeSpec)
import           HollaBack.Date.Conversion (decideTime)
import           HollaBack.Scheduler.Storage (persistHollaBack,
                                             pollForHollaBacks,
                                             getIncomingMessage,
                                             mailbox, --DEBUG
                                             dateTimeSpecFromEmail)
import           HollaBack.Scheduler.Types

main :: IO ()
main = sequence_ =<< mapM spawn [handleIncomingMessages, handleDueHollaBacks]

---- Helpers
--TODO: redis connection cleanup
handleIncomingMessages :: IO ()
handleIncomingMessages = do
  warn "started message handler"
  redis <- connect localhost defaultPort
  forever $ do
    pl <- getIncomingMessage redis
    either warn (persistHollaBack redis pl) $ dateTimeSpecFromEmail . to $ pl

handleDueHollaBacks :: IO ()
handleDueHollaBacks = do
  warn "started  due followups monitor"
  redis <- connect localhost defaultPort
  forever $ do
    pollForHollaBacks redis cb
    threadDelay delaySeconds
  disconnect redis
  where cb p         = BS.putStrLn $ "Got payload " `BS.append` (pack . show $ p)
        delaySeconds = 5000000 -- 5 seconds

warn :: String -> IO ()
warn = hPutStrLn stderr
