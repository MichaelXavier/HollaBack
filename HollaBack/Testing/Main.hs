module Main (main) where

import Test.Hspec (hspecX)

import qualified HollaBack.Testing.Date.Parser as DP (specs)
import qualified HollaBack.Testing.Date.Conversion as DC (specs)

main :: IO ()
main = hspecX $ DP.specs ++ DC.specs
