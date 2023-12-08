module Main where

import Data.Csv.THSpec qualified as Data.Csv.TH
import Test.Hspec

main :: IO ()
main = do
    hspec do
        describe "Data.Csv.TH" Data.Csv.TH.spec
