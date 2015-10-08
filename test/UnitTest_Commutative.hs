module Main where
import Test.HUnit

import DeepControl.Applicative
import DeepControl.Monad
import DeepControl.Commutative

main :: IO ()
main = do
    runTestTT $ TestList [
          TestList tTest0
  --      , TestList tLevel2

        ]
    runTestTT tests_tTest0
    return ()

----------------------------------------------------------------
-- unit test
----------------------------------------------------------------

tTest0 = ("tTest0" ~:) |$> [
       commute (Just [1]) ~?= [Just 1]
     ]

tests_tTest0 :: Test
tests_tTest0 = test [ 
      "IO" ~: do
        actual <- commute (return |$> [1..3])
        actual @?= [1,2,3]
    ]


{-
tests_Level2 :: Test
tests_Level2 = test [ 
      "List-List" ~: do
        let actual = [[1,2],[3,4]] >>== \x ->
                     [[5,6],[7,8]] >>== \y ->
                     (**:) $ x+y
        actual @?= [[6,7,7,8],[6,7,9,10],[8,9,7,8],[8,9,9,10],[8,9,9,10],[8,9,11,12],[10,11,9,10],[10,11,11,12]]

    , "List-Maybe" ~: "(>>==, >>_)" ~: do
        let actual = (Just |$> [1..10]) >>== \x ->
                     (Just |$> [1..10]) >>== \y ->
                     (Just |$> [1..10]) >>== \z -> 
                     ((*:) $ guard (x < y && x*x + y*y == z*z)) >>~
                     (**:) (x,y,z)
        filter isJust actual @?= [Just (3,4,5),Just (6,8,10)]
    , "List-Maybe" ~: "(>-==, ->_)" ~: do
        let actual = [1..10] >-== \x ->
                     [1..10] >-== \y ->
                     [1..10] >-== \z -> 
                     guard (x < y && x*x + y*y == z*z) ->~
                     (**:) (x,y,z)
        filter isJust actual @?= [Just (3,4,5),Just (6,8,10)]

    , "(->)-Maybe" ~:  do
        let lengthM :: [Int] -> Maybe Int
            lengthM [] = Nothing
            lengthM xs = Just (length xs) 
            averageM :: [Int] -> Maybe Double
            averageM = 
                sum >-== \s ->
                lengthM >>== \l ->
                (**:) $ fromIntegral s / fromIntegral l
        averageM [10, 25, 70] @?= Just 35.0
        averageM []           @?= Nothing
        
    , "Maybe-Writer" ~:  do
        let factorialM :: Int -> Maybe (Writer [Int] Int)
            factorialM n | n < 0  = (*:) |$> Nothing
            factorialM n | n == 0 = (*:) $ tell [0] >> return 1
            factorialM n | n > 0  = 
                factorialM (n-1) >>== \v ->
                tell [v] ->~
                (**:) $ n * v
        (runWriter |$> factorialM 5) @?= Just (120,[0,1,1,2,6,24])

    ]
  where
    isJust (Just _) = True
    isJust _        = False
-}
