module HollaBack.Date.Types (Tag(..),
                            Tags,
                            TimeUnit(..),
                            TimeKeyword(..),
                            HollaBackSpec(..),
                            Month(..),
                            DayOfWeek(..),
                            Date(..),
                            TimeOfDay(..),
                            DateTimeSpec(..),
                            dowNum,
                            monthNum) where

import Data.Text (Text)
import Data.Time.LocalTime (TimeOfDay(..))

data DayOfWeek = Monday    |
                 Tuesday   |
                 Wednesday |
                 Thursday  |
                 Friday    |
                 Saturday  |
                 Sunday deriving (Show, Eq)

instance Ord DayOfWeek where
  compare a b = compare (dowNum a) (dowNum b)


data HollaBackSpec = HollaBackSpec DateTimeSpec Tags deriving (Show, Eq)

data Month = January   |
             February  |
             March     |
             April     |
             May       |
             June      |
             July      |
             August    |
             September |
             October   |
             November  |
             December deriving (Show, Eq)

data Date = Date Month Int deriving (Show, Eq)

data DateTimeSpec = RelativeDateTime TimeUnit               |
                    SpecificDateTime Date TimeOfDay         |
                    SpecificWeekdayTime DayOfWeek TimeOfDay |
                    SpecificWeekday DayOfWeek               |
                    SpecificTime TimeOfDay deriving (Show, Eq)

type Tags = [Tag]

data Tag = Tag Text deriving (Show, Eq, Ord)

data TimeUnit = TimeUnit Integer TimeKeyword deriving (Show, Eq)

data TimeKeyword = Minutes |
                   Hours   |
                   Days    |
                   Weeks   |
                   Months  |
                   Years deriving (Show, Eq)

dowNum :: DayOfWeek -> Int
dowNum Monday    = 1
dowNum Tuesday   = 2
dowNum Wednesday = 3
dowNum Thursday  = 4
dowNum Friday    = 5
dowNum Saturday  = 6
dowNum Sunday    = 7

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
