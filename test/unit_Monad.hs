import Test.HUnit

import DeepControl.Applicative
import DeepControl.Monad
import DeepControl.Monad.Trans.Identity

import Control.Monad.Writer
import Control.Monad.Reader
import Safe

main :: IO ()
main = do
    runTestTT $ TestList [
          TestList tLevel0
        , TestList tLevel2
        , TestList tLevel3
        ]
    runTestTT tests_Level0
    putStrLn "" >> print "----------------"
    runTestTT tests_Level2
    putStrLn "" >> print "----------------"
    runTestTT tests_Level3
    putStrLn "" >> print "----------------"
    return ()

tLevel0 = ("Level0" ~:) |$>
    [ (3 >- Just)    ~?= Just 3
    , (Just -< 3)    ~?= Just 3

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

tests_Level0 :: Test
tests_Level0 = test [ 
      "plus" ~: "(>-)" ~: do
        let plus :: Int -> Int -> Int
            plus x y = 
                x >- \a ->
                y >- \b ->
                a + b
        plus 3 4 @?= 7
    ]

tests_Level2 :: Test
tests_Level2 = test [ 
      "List-List" ~: "(>>==)" ~: do
        let actual :: [[String]]
            actual = [["a","b"]] >>== \x ->
                     [[0],[1,2]] >>== \y ->
                     (**:) $ x ++ show y
        actual @?= [["a0","b0"],["a0","b1","b2"],["a1","a2","b0"],["a1","a2","b1","b2"]]

    , "List-Maybe" ~: "(>>==), (>>~)" ~: do
        let actual :: [Maybe Double]
            actual = (readMay |$> ["1", "2", "_"]) >>== \a ->
                     (readMay |$> ["0", "2"]) >>== \b ->
                     ((*:) $ guard (b /= 0.0)) >>~
                     (**:) $ a / b
        actual @?= [Just 0.5,Just 1.0,Nothing]
    , "List-Maybe" ~: "(->~)" ~: do
        let actual :: [Maybe Double]
            actual = (readMay |$> ["1", "2", "_"]) >>== \a ->
                     (readMay |$> ["0", "2"]) >>== \b ->
                     guard (b /= 0.0) ->~
                     (**:) $ a / b
        actual @?= [Just 0.5,Just 1.0,Nothing]

    , "(->)-Maybe" ~: "(>-==)" ~: do
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
            lengthRM = ReaderT $ \r -> Identity $ case r of
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
                        | n == 0 = (*:) $ tell [0] >> return 1
                        | n > 0  = factorial (n-1) >>== \v ->
                                   tell [v] ->~
                                   (**:) (n * v)
        (runWriter |$> factorial 5) @?= Just (120,[0,1,1,2,6,24])

    ]

tests_Level3 :: Test
tests_Level3 = test [ 
      "IO-Maybe-Writer" ~: "(>>>==), (-->~), (*-*)" ~: do
        let factorial :: Int -> IO (Maybe (Writer [Int] Int))
            factorial n | n < 0  = (*-*) Nothing
                        | n == 0 = (**:) $ tell [0] >> return 1
                        | n > 0  = factorial (n-1) >>>== \v ->
                                   print v >--~
                                   tell [v] -->~
                                   (***:) (n * v)
        actual <- factorial 5
        (runWriter |$> actual) @?= Just (120,[0,1,1,2,6,24])
    ]

