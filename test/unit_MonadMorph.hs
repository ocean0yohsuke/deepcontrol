import Test.HUnit

import DeepControl.Applicative
import DeepControl.Monad.Morph

import Control.Monad.List

main :: IO ()
main = do
    runTestTT $ TestList [
          TestList tLevel0
        ]
    return ()

f :: Maybe a -> [a]
f (Just x) = [x]
f Nothing = []

tLevel0 = ("Level0" ~:) |$>
    [ (f |>| ListT (Just [1,2,3]))    ~?= ListT [[1,2,3]]
    ]

