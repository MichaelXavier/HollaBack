{-# LANGUAGE OverloadedStrings #-}
module HollaBack.Date.Conversion (decideTimestamp,
                                  timestamp,
                                  dowDiff) where

import Control.Applicative ((<$>),
                            (<*>),
                            pure)
import Data.Time.Calendar (Day(..),
                           addDays,
                           addGregorianYearsRollOver,
                           addGregorianMonthsRollOver,
                           toGregorian,
                           fromGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Clock (UTCTime(..),
                        DiffTime,
                        getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.LocalTime (timeOfDayToTime,
                            midnight)

import HollaBack.Date.Types

decideTime :: DateTimeSpec -> IO UTCTime
decideTime (RelativeDateTime tu)         = offsetTime <$> tu'   <*> getCurrentTime
  where tu' = pure tu
decideTime (SpecificDateTime date tod)   = UTCTime    <$> day   <*> diffTime
  where diffTime = pure $ timeOfDayToTime tod
        day      = dateToDay date
decideTime (SpecificWeekdayTime dow tod) = UTCTime    <$> day   <*> diffTime
  where day      = dowToDay dow
        diffTime = pure $ timeOfDayToTime tod
decideTime (SpecificWeekday dow)         = UTCTime    <$> day   <*> diffTime
  where day      = dowToDay dow
        diffTime = pure startOfDay
decideTime (SpecificTime tod)            = UTCTime    <$> today <*> diffTime
  where diffTime = pure $ timeOfDayToTime tod

timestamp :: UTCTime -> Integer
timestamp = floor . utcTimeToPOSIXSeconds

decideTimestamp :: Int -> DateTimeSpec -> IO Integer
decideTimestamp _ dts@(RelativeDateTime _) = timestamp <$> decideTime dts
decideTimestamp offset dts                 = offsetTimestamp offset . timestamp <$> decideTime dts

-- We negate the sender's offset seconds to compensate it. If they are in
-- PST(-8:00) and they ask for 6AM, we store it as 2PM UTC
offsetTimestamp :: Int -> Integer -> Integer
offsetTimestamp secs = (+ compensatedOffset)
  where compensatedOffset = negate . fromIntegral $ secs

dowDiff :: DayOfWeek -> DayOfWeek -> Int
dowDiff start finish 
  | start > finish = dowNum finish + 7 - dowNum start
  | otherwise      = dowNum finish - dowNum start

addDow :: Day -> DayOfWeek -> Day
addDow start finish = addDays diff start
  where diff = toInteger $ adjustedFinish - startDowNum
        adjustedFinish
          | startDowNum > finishDowNum = finishDowNum + 7
          | otherwise                  = finishDowNum
        startDowNum = dayToDowNum start
        finishDowNum = dowNum finish

---- Helpers
today :: IO Day
today = utctDay <$> getCurrentTime

thisYear :: IO Integer
thisYear = getYear . toGregorian . utctDay <$> getCurrentTime
  where getYear (year, _, _) = year

dowToDay :: DayOfWeek -> IO Day
dowToDay dowFinish = addDow <$> today <*> finish
  where finish = pure dowFinish

dayToDowNum :: Day -> Int
dayToDowNum day = fromIntegral dn
  where (_, _, dn) = toWeekDate day

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
  where newDay = addDays ds day
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
