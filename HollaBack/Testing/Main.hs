module Main (main) where

import Test.Hspec (hspec)

import qualified HollaBack.Testing.Date.Parser as DP (specs)
import qualified HollaBack.Testing.Date.Conversion as DC (specs)

main :: IO ()
main = hspec $ DP.specs >> DC.specs
