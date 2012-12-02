{-# LANGUAGE OverloadedStrings #-}
module HollaBack.Testing.Date.Conversion (specs) where

import Test.Hspec (Spec,
                   describe,
                   it)

import Test.Hspec.HUnit
import Test.HUnit.Base ((~?=))

import HollaBack.Date.Types
import HollaBack.Date.Conversion

specs :: Spec
specs = describe_dowDiff

describe_dowDiff :: Spec
describe_dowDiff = describe "dowDiff" $ do
  it "returns 0 when dates are equivalent" $
    (dowDiff Tuesday Tuesday ~?= 0)
  it "returns the difference when finish is after start" $
    (dowDiff Tuesday Sunday ~?= 5)
  it "rolls over to the next week when start is after finish" $
    (dowDiff Saturday Wednesday ~?= 4)
