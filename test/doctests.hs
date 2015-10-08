module Main where

import Test.DocTest
--import Test.QuickCheck

main :: IO ()
main = doctest [ "DeepControl/Applicative.hs"
               , "DeepControl/Monad.hs"
               , "DeepControl/Arrow.hs"
               ]
