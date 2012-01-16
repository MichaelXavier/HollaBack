{-# LANGUAGE OverloadedStrings #-}
module HollaBack.Date.Conversion (decideTime,
                                 timestamp) where

import Control.Applicative ((<$>),
                            (<*>),
                            pure)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Time.Calendar (Day(..),
                           addDays,
                           addGregorianYearsRollOver,
                           addGregorianMonthsRollOver,
                           toGregorian,
                           fromGregorian)
import Data.Time.Clock (UTCTime(..),
                        DiffTime,
                        getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.LocalTime (timeOfDayToTime,
                            midnight)

import HollaBack.Date.Types

decideTime :: DateTimeSpec -> IO UTCTime
decideTime (RelativeDateTime tu)         = offsetTime <$> tu' <*> getCurrentTime
  where tu' = pure tu
decideTime (SpecificDateTime date tod)   = UTCTime <$> day <*> diffTime
  where diffTime = pure $ timeOfDayToTime tod
        day      = dateToDay date
decideTime (SpecificWeekdayTime dow tod) = UTCTime <$> day <*> diffTime
  where day      = dowToDay dow
        diffTime = pure $ timeOfDayToTime tod
decideTime (SpecificWeekday dow)         = UTCTime <$> day   <*> diffTime
  where day      = dowToDay dow
        diffTime = pure startOfDay
decideTime (SpecificTime tod)            = UTCTime <$> today <*> diffTime
  where diffTime = pure $ timeOfDayToTime tod

timestamp :: UTCTime -> Integer
timestamp = floor . utcTimeToPOSIXSeconds

---- Helpers
today :: IO Day
today = utctDay <$> getCurrentTime

thisYear :: IO Integer
thisYear = getYear . toGregorian . utctDay <$> getCurrentTime
  where getYear (year, _, _) = year


dowToDay :: DayOfWeek -> IO Day
dowToDay = undefined

startOfDay :: DiffTime 
startOfDay = timeOfDayToTime midnight

dateToDay :: Date -> IO Day
dateToDay (Date month dom) = fromGregorian <$> year <*> month' <*> dom'
  where year   = thisYear
        month' = pure $ monthNum month
        dom'   = pure dom

offsetTime :: TimeUnit -> UTCTime -> UTCTime
offsetTime (TimeUnit ms Minutes)
           utct@UTCTime { utctDayTime = dt } = utct { utctDayTime = newTime }
  where newTime = dt + seconds
        seconds = fromInteger $ ms * 60
offsetTime (TimeUnit hs Hours)
           utct@UTCTime { utctDayTime = dt } = utct { utctDayTime = newTime }
  where newTime = dt + seconds
        seconds = fromInteger $ hs * 60 * 60
offsetTime (TimeUnit ds Days)
           utct@UTCTime { utctDay = day }    = utct { utctDay = newDay }
  where newDay = addDays ds newDay
offsetTime (TimeUnit ws Weeks)
           utct@UTCTime { utctDay = day }    = utct  { utctDay = newDay }
  where newDay = addDays days day
        days   = ws * 7
offsetTime (TimeUnit ms Months)
           utct@UTCTime { utctDay = day }    = utct { utctDay = newDay }
  where newDay = addGregorianMonthsRollOver ms day
offsetTime (TimeUnit ys Years)
           utct@UTCTime { utctDay = day }    = utct { utctDay = newDay }
  where newDay = addGregorianYearsRollOver ys day

monthNum :: Month -> Int
monthNum January   = 1
monthNum February  = 2
monthNum March     = 3
monthNum April     = 4
monthNum May       = 5
monthNum June      = 6
monthNum July      = 7
monthNum August    = 8
monthNum September = 9
monthNum October   = 10
monthNum November  = 11
monthNum December  = 12
