module Main where

import           Test.DocTest
--import Test.QuickCheck

main :: IO ()
main = doctest [ "DeepControl/Applicative.hs"
               , "DeepControl/Traversable.hs"
               , "DeepControl/Monad.hs"
               , "DeepControl/Monad/Morph.hs"
               , "DeepControl/Monad/Trans.hs"
               , "DeepControl/Monad/Trans/Except.hs"
               , "DeepControl/Monad/Trans/Writer.hs"
               , "DeepControl/Monad/Trans/Identity.hs"
               , "DeepControl/Arrow.hs"
               ]
