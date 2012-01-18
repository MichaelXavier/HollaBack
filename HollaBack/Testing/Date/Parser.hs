{-# LANGUAGE OverloadedStrings #-}
module HollaBack.Testing.Date.Parser (specs) where

import Data.Attoparsec.Text (parseOnly,
                             maybeResult,
                             eitherResult,
                             Parser)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Test.Hspec (Specs,
                   describe,
                   descriptions,
                   pending,
                   it)
import Test.Hspec.HUnit
import Test.HUnit.Base ((~?=))

import HollaBack.Date.Types
import HollaBack.Date.Parser

specs :: Specs
specs = descriptions [describe_tag,
                      describe_tags,
                      describe_timeUnit,
                      describe_timeKeyword,
                      describe_day,
                      describe_date,
                      describe_dateTimeSpec,
                      describe_time]

describe_tag :: Specs
describe_tag =
  describe "tag" [
    it "Parses a well-formed tag"
    (parseOnly tag "+Work" ~?= Right work),
    it "Does not consume past a second +"
    (parseOnly tag "+Work+School" ~?= Right work),
    it "Does not parse raw strings"
    (fails $ parseOnly tag "Work")
  ]

describe_tags :: Specs
describe_tags =
  describe "tags" [
    it "Parses an empty string as an empty list"
    (parseOnly tags "" ~?= Right []),
    it "Parses 1 tag"
    (parseOnly tags "+Work" ~?= Right [work]),
    it "Parses many tags"
    (parseOnly tags "+Work+School" ~?= Right [work, school])
  ]

describe_timeKeyword :: Specs
describe_timeKeyword =
  describe "timeKeyword" [
    it "parses minutes"
    (parseOnly timeKeyword "mi" ~?= Right Minutes),
    it "parses hours"
    (parseOnly timeKeyword "h" ~?= Right Hours),
    it "parses days"
    (parseOnly timeKeyword "d" ~?= Right Days),
    it "parses weeks"
    (parseOnly timeKeyword "w" ~?= Right Weeks),
    it "parses months"
    (parseOnly timeKeyword "mo" ~?= Right Months),
    it "parses years"
    (parseOnly timeKeyword "y" ~?= Right Years),
    it "fails to parse other tokens"
    (fails $ parseOnly timeKeyword "z")
  ]

describe_timeUnit :: Specs
describe_timeUnit =
  describe "timeUnit" [
    it "parses well formed TimeUnits"
    (parseOnly timeUnit "10d" ~?= Right (TimeUnit 10 Days)),
    it "fails decimal days"
    (fails $ parseOnly timeKeyword "10.5d")
  ]

describe_day :: Specs
describe_day =
  describe "day" [
    it "parses mon"
    (parseOnly day "mon" ~?= Right Monday),
    it "parses monday"
    (parseOnly day "monday" ~?= Right Monday),
    it "parses tue"
    (parseOnly day "tue" ~?= Right Tuesday),
    it "parses tues"
    (parseOnly day "tues" ~?= Right Tuesday),
    it "parses tuesday"
    (parseOnly day "tuesday" ~?= Right Tuesday),
    it "parses wed"
    (parseOnly day "wed" ~?= Right Wednesday),
    it "parses wednesday"
    (parseOnly day "wednesday" ~?= Right Wednesday),
    it "parses thu"
    (parseOnly day "thu" ~?= Right Thursday),
    it "parses thur"
    (parseOnly day "thur" ~?= Right Thursday),
    it "parses thurs"
    (parseOnly day "thurs" ~?= Right Thursday),
    it "parses thursday"
    (parseOnly day "thursday" ~?= Right Thursday),
    it "parses fri"
    (parseOnly day "fri" ~?= Right Friday),
    it "parses friday"
    (parseOnly day "friday" ~?= Right Friday),
    it "parses sat"
    (parseOnly day "sat" ~?= Right Saturday),
    it "parses saturday"
    (parseOnly day "saturday" ~?= Right Saturday),
    it "parses sun"
    (parseOnly day "sun" ~?= Right Sunday),
    it "parses sunday"
    (parseOnly day "sunday" ~?= Right Sunday)
  ]

describe_date :: Specs
describe_date =
  describe "date" [
    it "parses well-formed dates" 
    (parseOnly date "may25" ~?= Right (Date May 25)),
    it "parses with leading 0s" 
    (parseOnly date "mar05" ~?= Right (Date March 5)),
    it "parses full month names" 
    (parseOnly date "january3" ~?= Right (Date January 3)),
    it "is case insensitive"
    (parseOnly date "January3" ~?= Right (Date January 3))
  ]

describe_time :: Specs
describe_time =
  describe "time" [
    it "parses AM times"
    (parseOnly time "3am" ~?= Right threeAm),
    it "parses PM times"
    (parseOnly time "9pm" ~?= Right ninePm),
    it "is case insensitive about the time half"
    (parseOnly time "9PM" ~?= Right ninePm),
    it "fails on out-of-bounds times"
    (pending "implement bounds checks")
  ]
  where threeAm = TimeOfDay 3 0 0
        ninePm  = TimeOfDay 21 0 0

describe_dateTimeSpec :: Specs
describe_dateTimeSpec =
  describe "dateTimeSpec" [
    it "parses RelativeDateTime"
    (parseOnly dateTimeSpec "13d" ~?= Right relativeDateTime),
    it "parses SpecificDateTime"
    (parseOnly dateTimeSpec "Mar13-6pm" ~?= Right specificDateTime),
    it "parses SpecificWeekdayTime"
    (parseOnly dateTimeSpec "thurs-1pm" ~?= Right specificWeekdayTime),
    it "parses SpecificWeekday"
    (parseOnly dateTimeSpec "fri" ~?= Right specificWeekday),
    it "parses SpecificTime"
    (parseOnly dateTimeSpec "5AM" ~?= Right specificTime)
  ]
  where relativeDateTime    = RelativeDateTime $ TimeUnit 13 Days
        specificDateTime    = SpecificDateTime (Date March 13) $ TimeOfDay 18 0 0
        specificWeekdayTime = SpecificWeekdayTime Thursday $ TimeOfDay 13 0 0
        specificWeekday     = SpecificWeekday Friday
        specificTime        = SpecificTime $ TimeOfDay 5 0 0

---- Fixtures
work :: Tag
work = Tag "Work"

school :: Tag
school = Tag "School"

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

fails res = isLeft res ~?= True
