module HollaBack.Date.Types (Tag(..),
                            Tags,
                            TimeUnit(..),
                            TimeKeyword(..),
                            HollaBackSpec(..),
                            Month(..),
                            DayOfWeek(..),
                            Date(..),
                            TimeOfDay(..),
                            DateTimeSpec(..)) where

import Data.Text (Text)
import Data.Time.LocalTime (TimeOfDay(..))
import Time (Month(..))

data DayOfWeek = Monday    |
                 Tuesday   |
                 Wednesday |
                 Thursday  |
                 Friday    |
                 Saturday  |
                 Sunday deriving (Show, Eq)


data HollaBackSpec = HollaBackSpec DateTimeSpec Tags deriving (Show, Eq)

data Date = Date Month Int deriving (Show, Eq)

data DateTimeSpec = RelativeDateTime TimeUnit         |
                    SpecificDateTime Date TimeOfDay   |
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
