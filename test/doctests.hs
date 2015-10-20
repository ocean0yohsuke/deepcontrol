module Main where

import Test.DocTest
--import Test.QuickCheck

main :: IO ()
main = doctest [ "DeepControl/Applicative.hs"
               , "DeepControl/Commutative.hs"
               , "DeepControl/Monad.hs"
               , "DeepControl/Monad/Trans.hs"
               , "DeepControl/Monad/Morph.hs"
               , "DeepControl/Arrow.hs"
               ]
