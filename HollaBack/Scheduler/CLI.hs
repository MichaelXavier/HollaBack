{-# LANGUAGE QuasiQuotes, DeriveDataTypeable, OverloadedStrings #-}
module HollaBack.Scheduler.CLI (annotations,
                                Config(..)) where

import Data.Data (Data)
import Data.String.QQ (s)
import Data.Typeable (Typeable)
import Database.Redis.Redis (localhost,
                             defaultPort)
import System.Console.CmdArgs.Implicit (Annotate((:=)),
                                        Ann,
                                        (+=),
                                        help,
                                        name,
                                        summary,
                                        record,
                                        typ,
                                        program)

annotations :: Annotate Ann
annotations = record Config { redisPort     = defaultRedisPort,
                              redisHost     = defaultRedisHost,
                              mailInterval  = defaultMailInterval }
                     [ redisPort    := defaultRedisPort
                                    += name "redis-port"
                                    += help ("Redis port. Default is " ++ defaultRedisPort)
                                    += typ "PORT",
                       redisHost    := defaultRedisHost
                                    += name "redis-host"
                                    += help ("Redis host. Default " ++ defaultRedisHost)
                                    += typ "HOST",
                       mailInterval := defaultMailInterval
                                    += name "i"
                                    += help ("Frequency to check for mail to send in seconds. Default " ++ show defaultMailInterval)
                                    += typ "SECONDS"]
                     += program "hollaback"
                     += summary summ
  where summ = [s|
hollaback email responder and scheduler. More description to come.
  |]

data Config = Config { redisPort     :: String,
                       redisHost     :: String,
                       mailInterval  :: Int } deriving (Show, Data, Typeable, Eq)

---- Helpers

defaultMailInterval :: Int
defaultMailInterval = 5

defaultRedisPort :: String
defaultRedisPort = defaultPort

defaultRedisHost :: String
defaultRedisHost = localhost
