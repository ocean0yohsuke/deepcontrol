import Test.HUnit

import DeepControl.Applicative
import DeepControl.Monad
import DeepControl.Traversable

main :: IO ()
main = do
    runTestTT $ TestList [
          TestList tTest0
        ]
    runTestTT tests_0
    return ()

----------------------------------------------------------------
-- unit test
----------------------------------------------------------------

tTest0 = ("tTest0" ~:) |$> [
       sink (Just [1]) ~?= [Just 1]
     ]

tests_0 :: Test
tests_0 = test [ 
      "IO" ~: do
        actual <- sink ((-*) [1..3])
        actual @?= [1,2,3]
    ]


