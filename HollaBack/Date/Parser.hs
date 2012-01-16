{-# LANGUAGE OverloadedStrings #-}
module HollaBack.Date.Parser (tag,
                             tags,
                             timeUnit,
                             timeKeyword,
                             day,
                             date,
                             dateTimeSpec,
                             time) where

import Control.Applicative ((<*>),
                            (*>),
                            (<$>),
                            (<$),
                            (<|>),
                            pure,
                            many)
import qualified Data.Attoparsec.Text as A
import qualified Data.Attoparsec.Combinator as AC
import Data.Attoparsec.Text (Parser)
import Data.Text (Text)
import Data.Time.Clock (secondsToDiffTime)
import Data.Word (Word8)

import HollaBack.Date.Types

-- Date format specifications:
--[quantity][time keyword]@followup.cc
-- mi = minutes, h = hours, d = days, w = weeks, mo = months, y = years

-- Future day of week: [weekday]-[time]?@followup.cc
-- Future date: [month][day]-[time]@followup.cc
-- Upcoming hour: [hour]@followup.cc

-- Tags: [datetimespec]+tag1+tag2@followup.cc

-- Grammar:
-- HollaBackSpec ::= <DateTime><Tags>
-- DateTime ::= <RelativeDateTime> |
--              <SpecificDateTime>
-- SpecificDateTime ::= <Month><Day>|
--                      <Month><Day>-<Hour> |
--                      <DayOfWeek>-<Hour> |
--                      <DayOfWeek> |
--                      <Hour>
-- FutureDateTime ::= <Integer><TimeUnit>
-- TimeUnit ::= mi | h | d | w | mo | y
-- Tags ::= "" | +<String> | <Tags><Tag>

-- Text looks like +Work
tag :: Parser Tag
tag = A.char '+' *> (Tag <$> A.takeWhile (/= '+'))

tags :: Parser Tags
tags = many tag

timeUnit :: Parser TimeUnit
timeUnit = TimeUnit <$> (toInteger <$> A.decimal)
                    <*> timeKeyword

timeKeyword :: Parser TimeKeyword
timeKeyword = minutes <|>
              hours   <|>
              days    <|>
              weeks   <|>
              months  <|>
              years
  where minutes = A.stringCI "mi" *> pure Minutes
        hours   = A.stringCI "h"  *> pure Hours
        days    = A.stringCI "d"  *> pure Days
        weeks   = A.stringCI "w"  *> pure Weeks
        months  = A.stringCI "mo" *> pure Months
        years   = A.stringCI "y"  *> pure Years

day :: Parser DayOfWeek
day = monday    <|>
      tuesday   <|>
      wednesday <|>
      thursday  <|>
      friday    <|>
      saturday  <|>
      sunday
 where monday    = stringChoices ["monday", "mon"]                    *> pure Monday
       tuesday   = stringChoices ["tuesday", "tues", "tue"]           *> pure Tuesday
       wednesday = stringChoices ["wednesday", "wed"]                 *> pure Wednesday
       thursday  = stringChoices ["thursday", "thurs", "thur", "thu"] *> pure Thursday
       friday    = stringChoices ["friday", "fri"]                    *> pure Friday
       saturday  = stringChoices ["saturday", "sat"]                  *> pure Saturday
       sunday    = stringChoices ["sunday", "sun"]                    *> pure Sunday

time :: Parser TimeOfDay
time = toTime <$> simpleTime
  where toTime (hours, AM) = TimeOfDay hours 0 0
        toTime (hours, PM) = TimeOfDay (hours + 12) 0 0

date :: Parser Date
date = Date <$> month <*> A.decimal

--TODO: needs dashes
dateTimeSpec :: Parser DateTimeSpec
dateTimeSpec = relativeDateTime    <|>
               specificDateTime    <|>
               specificWeekdayTime <|>
               specificWeekday     <|>
               specificTime
  where relativeDateTime    = RelativeDateTime    <$> timeUnit
        specificDateTime    = SpecificDateTime    <$> date <*> dashTime
        specificWeekdayTime = SpecificWeekdayTime <$> day <*> dashTime
        specificWeekday     = SpecificWeekday     <$> day
        specificTime        = SpecificTime        <$> time
        dashTime            = A.char '-' *> time

---- Helpers

stringChoices :: [Text] -> Parser Text
stringChoices = AC.choice . map A.stringCI

data DayHalf = AM | PM

type SimpleTime = (Int, DayHalf)

simpleTime :: Parser SimpleTime
simpleTime = (,) <$> A.decimal <*> dayHalf

dayHalf :: Parser DayHalf
dayHalf = am <|> pm
  where am = A.stringCI "am" *> pure AM
        pm = A.stringCI "pm" *> pure PM

month :: Parser Month
month = january   <|>
        february  <|>
        march     <|>
        april     <|>
        may       <|>
        june      <|>
        july      <|>
        august    <|>
        september <|>
        october   <|>
        november  <|>
        december
  where january   = stringChoices ["january", "jan"]   *> pure January
        february  = stringChoices ["february", "feb"]  *> pure February
        march     = stringChoices ["march", "mar"]     *> pure March
        april     = stringChoices ["april", "apr"]     *> pure April
        may       = stringChoices ["may"]              *> pure May
        june      = stringChoices ["june", "jun"]      *> pure June
        july      = stringChoices ["july", "jul"]      *> pure July
        august    = stringChoices ["august", "aug"]    *> pure August
        september = stringChoices ["september", "sep"] *> pure September
        october   = stringChoices ["october", "oct"]   *> pure October
        november  = stringChoices ["november", "nov"]  *> pure November
        december  = stringChoices ["december", "dec"]  *> pure December
