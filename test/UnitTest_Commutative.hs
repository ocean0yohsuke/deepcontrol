import Test.HUnit

import DeepControl.Applicative
import DeepControl.Monad
import DeepControl.Commutative

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
       commute (Just [1]) ~?= [Just 1]
     ]

tests_0 :: Test
tests_0 = test [ 
      "IO" ~: do
        actual <- commute ((-*) [1..3])
        actual @?= [1,2,3]
    ]


