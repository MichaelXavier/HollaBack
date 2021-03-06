{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Spawn (spawn)
import           Control.Monad (forever)
import           Database.Redis.Redis
import           System.Console.CmdArgs.Implicit (cmdArgs_)
import           System.IO (hPutStrLn,
                            stderr)

import           HollaBack.Scheduler.Storage (persistHollaBack,
                                              pollForHollaBacks,
                                              getIncomingMessage,
                                              dateTimeSpecFromEmail)
import           HollaBack.Mailer (hollaBack)
import           HollaBack.Types
import           HollaBack.Scheduler.CLI (annotations,
                                          Config(..))


main :: IO ()
main = runWithConfig =<< cmdArgs_ annotations

runWithConfig :: Config -> IO ()
runWithConfig Config { redisPort = port,
                       redisHost = host,
                       mailInterval = interval} = sequence_ =<< mapM spawn [handleIncoming, handleDue]
  where handleIncoming = handleIncomingMessages host port
        handleDue      = handleDueHollaBacks host port interval

---- Helpers
--TODO: redis connection cleanup
handleIncomingMessages :: String -> String -> IO ()
handleIncomingMessages host port = do
  warn "started message handler"
  redis <- connect host port
  _ <- forever $ do
    pl <- getIncomingMessage redis
    either warn (persistHollaBack redis pl) $ dateTimeSpecFromEmail . to $ pl
  disconnect redis

handleDueHollaBacks :: String -> String -> Int -> IO ()
handleDueHollaBacks host port interval = do
  warn "started due followups monitor"
  redis <- connect host port
  _ <- forever $ do
    pollForHollaBacks redis hollaBack
    threadDelay delaySeconds
  disconnect redis
  where delaySeconds = interval * 1000000 -- 5 seconds

warn :: String -> IO ()
warn = hPutStrLn stderr
