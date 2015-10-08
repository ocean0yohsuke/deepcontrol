module Main where
import Test.HUnit

import DeepControl.Applicative
import DeepControl.Monad

import DeepControl.Monad.Writer
import DeepControl.Monad.Reader

main :: IO ()
main = do
    runTestTT $ TestList [
          TestList tLevel0
        , TestList tLevel2
        , TestList tLevel3
        ]
    runTestTT tests_Level2
    runTestTT tests_Level3
    return ()

tLevel0 = ("Level0" ~:) |$>
    [ (3 >- Just)    ~?= (Just 3)
    , (Just -< 3)    ~?= (Just 3)

    , (1 >- (+1) >- (*2) >- (+3))     ~?= 7
    , (1 >- ((+1) >-> (*2) >-> (+3))) ~?= 7
    , (((3+) <-< (2*) <-< (1+)) -< 1) ~?= 7

-- invalid forms
-- ((3+) -< (2*) -< (1+) -< 1)
    ]

tLevel2 = ("Level2" ~:) |$> [
      ([[1]] >>== \x -> [[x]]) ~?= [[1]]
    , ([[1]] >>== \x -> (**:) x) ~?= [[1]]
    , (Just [1] >>== \x -> (**:) x) ~?= Just [1]
    , ([Just 1] >>== \x -> (**:) x) ~?= [Just 1]
    , (Right [1] >>== \x -> (**:) x) ~?= (Right [1] :: Either () [Int])

    , (Right [0] >>== \x -> (**:) (x+1) >>== \x -> (**:) (x+2)) ~?= (Right [3] :: Either () [Int])
    ]

tLevel3 = ("Level3" ~:) |$> [
      ([[[1]]] >>>== \x -> (***:) x) ~?= [[[1]]]
    , ((Just [[1]]) >>>== \x -> (***:) x) ~?= Just [[1]]
    , (([Just [1]]) >>>== \x -> (***:) x) ~?= [Just [1]]
    , (Right (Just [1]) >>>== \x -> (***:) x) ~?= (Right (Just [1]) :: Either () (Maybe [Int]))

    , (Right (Just [0]) >>>== \x -> (***:) (x+1) >>>== \x -> (***:) (x+2)) ~?= (Right (Just [3]) :: Either () (Maybe [Int]))
    , (Right Nothing    >>>== \x -> (***:) (x+1) >>>== \x -> (***:) (x+2)) ~?= (Right Nothing :: Either () (Maybe [Int]))
    ]

tests_Level2 :: Test
tests_Level2 = test [ 
      "List-List" ~: "(>>==)" ~: do
        let actual = [["a","b"]] >>== \x ->
                     [[0],[1,2]] >>== \y ->
                     (**:) $ x ++ show y
        actual @?= [["a0","b0"],["a0","b1","b2"],["a1","a2","b0"],["a1","a2","b1","b2"]]

    , "List-Maybe" ~: "(>>==), (>>~)" ~: do
        let actual = (Just |$> [1..10]) >>== \x ->
                     (Just |$> [1..10]) >>== \y ->
                     (Just |$> [1..10]) >>== \z -> 
                     ((*:) $ guard (x < y && x*x + y*y == z*z)) >>~
                     (**:) (x,y,z)
        filter isJust actual @?= [Just (3,4,5),Just (6,8,10)]
    , "List-Maybe" ~: "(>-==), (->~)" ~: do
        let actual = [1..10] >-== \x ->
                     [1..10] >-== \y ->
                     [1..10] >-== \z -> 
                     guard (x < y && x*x + y*y == z*z) ->~
                     (**:) (x,y,z)
        filter isJust actual @?= [Just (3,4,5),Just (6,8,10)]

    , "(->)-Maybe" ~: do
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
    , "Reader-Maybe" ~: do
        let sumR :: Reader [Int] Int
            sumR = sum |$> ask
            lengthRM :: Reader [Int] (Maybe Int)
            lengthRM = Reader $ \r -> case r of
                                        [] -> Nothing
                                        xs -> Just (length xs) 
            averageRM :: Reader [Int] (Maybe Double)
            averageRM = 
                sumR >-== \s ->
                lengthRM >>== \l ->
                (**:) $ fromIntegral s / fromIntegral l
        runReader averageRM [10, 25, 70] @?= Just 35.0
        runReader averageRM []           @?= Nothing

    , "Maybe-Writer" ~: "(-*)" ~: do
        let factorial :: Int -> Maybe (Writer [Int] Int)
            factorial n | n < 0  = (-*) Nothing
            factorial n | n == 0 = (*:) $ tell [0] >> return 1
            factorial n | n > 0  = 
                factorial (n-1) >>== \v ->
                tell [v] ->~
                (**:) (n * v)
        (runWriter |$> factorial 5) @?= Just (120,[0,1,1,2,6,24])

    ]
  where
    isJust (Just _) = True
    isJust _        = False

tests_Level3 :: Test
tests_Level3 = test [ 
      "IO-Maybe-Writer" ~: "(>>>==), (-->~), (*-*)" ~: do
        let factorial :: Int -> IO (Maybe (Writer [Int] Int))
            factorial n | n < 0  = (*-*) Nothing
            factorial n | n == 0 = (**:) $ tell [0] >> return 1
            factorial n | n > 0  = 
                factorial (n-1) >>>== \v ->
                tell [v] -->~
                (***:) (n * v)
        actual <- factorial 5
        (runWriter |$> actual) @?= Just (120,[0,1,1,2,6,24])
    ]

