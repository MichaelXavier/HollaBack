module Main (main) where

import Test.Hspec (hspecX)

import qualified HollaBack.Testing.Date.Parser as DP (specs)

main :: IO ()
main = hspecX $ DP.specs
