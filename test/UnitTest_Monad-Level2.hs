import Test.HUnit hiding (State)

import DeepControl.Applicative ((*:))
import DeepControl.Monad ((>-))
import DeepControl.Commutative (cmap)
import DeepControl.Monad.Trans (liftIO, trans, trans2)
import DeepControl.Monad.Trans.State

import Control.Monad.Trans.Maybe

-----------------------------------------------
-- State

push :: a -> State [a] a
push x = do 
    xs <- get
    put (x:xs)
    return x

pop :: State [a] a
pop = do 
    xs <- get
    put (tail xs)
    return (head xs)

-- > runState (push 1 >> push 2 >> push 3) []
-- (3,[3,2,1])
-- > runState (push 1 >> push 2 >> push 3 >> pop >> pop) []
-- (2,[1])

poland :: String -> State [Double] Double
poland "+" = do 
    x <- pop
    y <- pop
    push (y + x)
poland "-" = do 
    x <- pop
    y <- pop
    push (y - x)
poland "*" = do 
    x <- pop
    y <- pop
    push (y * x)
poland "/" = do
    x <- pop
    y <- pop
    push (y / x)
poland x = push (read x :: Double)

poland_calc :: [String] -> (Double, [Double])
poland_calc xs = (cmap poland xs >> pop) >- \x -> runState x []

-- > poland_calc ["1","2","*"]
-- (2.0, [])
-- > poland_calc ["1","2","-"]
-- (-1.0, [])
-- > poland_calc ["1","2","+","3","*"]
-- (9.0, [])
-- > poland_calc ["1","2","+","3","*","3","/"]
-- (3.0, [])
-- > poland_calc ["1","2","+","3","*","0","/"]
-- (Infinity, [])

-----------------------------------------------
-- StateT-Maybe

pushS :: a -> StateT [a] Maybe a
pushS x = do 
    xs <- get
    put (x:xs)
    return x

popS :: StateT [a] Maybe a
popS = do 
    xs <- get
    put (tail xs)
    return (head xs)

-- > runStateT (pushT 1 >> pushT 2 >> pushT 3) []
-- Just (3,[3,2,1])
-- > runStateT (pushT 1 >> pushT 2 >> pushT 3 >> popT >> popT) []
-- Just (2,[1])

polandS :: String -> StateT [Double] Maybe Double
polandS "+" = do 
    x <- popS
    y <- popS
    pushS (y + x)
polandS "-" = do 
    x <- popS
    y <- popS
    pushS (y - x)
polandS "*" = do 
    x <- popS
    y <- popS
    pushS (y * x)
polandS "/" = do
    x <- popS
    y <- popS
    trans $ guard (x /= 0)
    pushS (y / x)
polandS x = pushS (read x :: Double)

poland_calcS :: [String] -> Maybe (Double, [Double])
poland_calcS xs = (cmap polandS xs >> popS) >- \x -> runStateT x []

-- > poland_calcS ["1","2","*"]
-- Just (2.0, [])
-- > poland_calcS ["1","2","-"]
-- Just (-1.0, [])
-- > poland_calcS ["1","2","+","3","*"]
-- Just (9.0, [])
-- > poland_calcS ["1","2","+","3","*","3","/"]
-- Just (3.0, [])
-- > poland_calcS ["1","2","+","3","*","0","/"]
-- Nothing

-----------------------------------------------
-- MaybeT-StateT-IO Monad

pushMS :: a -> MaybeT (StateT [a] IO) a
pushMS x = do 
    xs <- get
    put (x:xs)
    return x
popMS :: MaybeT (StateT [a] IO) a
popMS = do 
    xs <- get
    put (tail xs)
    return (head xs)

polandMS :: String -> MaybeT (StateT [Double] IO) Double
polandMS "+" = do 
    x <- popMS
    y <- popMS
    liftIO $ putStrLn (show y ++" + "++ show x ++" = "++ show (y + x))
    pushMS (y + x)
polandMS "-" = do 
    x <- popMS
    y <- popMS
    liftIO $ putStrLn (show y ++" - "++ show x ++" = "++ show (y - x))
    pushMS (y - x)
polandMS "*" = do
    x <- popMS
    y <- popMS
    liftIO $ putStrLn (show y ++" * "++ show x ++" = "++ show (y * x))
    pushMS (y * x)
polandMS "/" = do
    x <- popMS
    y <- popMS
    liftIO $ putStr (show y ++" / "++ show x ++" = ")
    guard (x /= 0)
    liftIO $ putStr (show (y / x) ++"\n")
    pushMS (y / x)
polandMS x = pushMS (read x :: Double)

poland_calcMS :: [String] -> IO (Maybe Double, [Double])
poland_calcMS xs = (cmap polandMS xs >> popMS) >- runMaybeT
                                               >- \x -> runStateT x []

-- > poland_calcS2 ["1","2","*"]
-- 1.0 * 2.0 = 2.0
-- (Just 2.0, [])

-----------------------------------------------
-- StateT-MaybeT-IO Monad

pushSM :: a -> StateT [a] (MaybeT IO) a
pushSM x = do 
    xs <- get
    put (x:xs)
    return x
popSM :: StateT [a] (MaybeT IO) a
popSM = do 
    xs <- get
    put (tail xs)
    return (head xs)

polandSM :: String -> StateT [Double] (MaybeT IO) Double
polandSM "+" = do 
    x <- popSM
    y <- popSM
    liftIO $ putStrLn (show y ++" + "++ show x ++" = "++ show (y + x))
    pushSM (y + x)
polandSM "-" = do 
    x <- popSM
    y <- popSM
    liftIO $ putStrLn (show y ++" - "++ show x ++" = "++ show (y - x))
    pushSM (y - x)
polandSM "*" = do
    x <- popSM
    y <- popSM
    liftIO $ putStrLn (show y ++" * "++ show x ++" = "++ show (y * x))
    pushSM (y * x)
polandSM "/" = do
    x <- popSM
    y <- popSM
    liftIO $ putStr (show y ++" / "++ show x ++" = ")
    guard (x /= 0)
    liftIO $ putStr (show (y / x) ++"\n")
    pushSM (y / x)
polandSM x = pushSM (read x :: Double)

poland_calcSM :: [String] -> IO (Maybe (Double, [Double]))
poland_calcSM xs = (cmap polandSM xs >> popSM) >- \x -> runStateT x []
                                               >- runMaybeT

-----------------------------------------------
-- StateT2-IO-Maybe

pushS2 :: a -> StateT2 [a] IO Maybe a
pushS2 x = do 
    xs <- get
    put (x:xs)
    return x
popS2 :: StateT2 [a] IO Maybe a
popS2 = do 
    xs <- get
    put (tail xs)
    return (head xs)

polandS2 :: String -> StateT2 [Double] IO Maybe Double
polandS2 "+" = do 
    x <- popS2
    y <- popS2
    liftIO $ putStrLn (show y ++" + "++ show x ++" = "++ show (y + x))
    pushS2 (y + x)
polandS2 "-" = do 
    x <- popS2
    y <- popS2
    liftIO $ putStrLn (show y ++" - "++ show x ++" = "++ show (y - x))
    pushS2 (y - x)
polandS2 "*" = do
    x <- popS2
    y <- popS2
    liftIO $ putStrLn (show y ++" * "++ show x ++" = "++ show (y * x))
    pushS2 (y * x)
polandS2 "/" = do
    x <- popS2
    y <- popS2
    liftIO $ putStr (show y ++" / "++ show x ++" = ")
    trans2 $ (*:) $ guard (x /= 0)
    liftIO $ putStr (show (y / x) ++"\n")
    pushS2 (y / x)
polandS2 x = pushS2 (read x :: Double)

poland_calcS2 :: [String] -> IO (Maybe (Double, [Double]))
poland_calcS2 xs = (cmap polandS2 xs >> popS2) >- \x -> runStateT2 x []

-- > poland_calcS2 ["1","2","*"]
-- 1.0 * 2.0 = 2.0
-- Just (2.0, [])
-- > poland_calcS2 ["1","2","+","3","*"]
-- 1.0 + 2.0 = 3.0
-- 3.0 * 3.0 = 9.0
-- Just (9.0, [])
-- > poland_calcS2 ["1","2","+","3","*","3","/"]
-- 1.0 + 2.0 = 3.0
-- 3.0 * 3.0 = 9.0
-- 9.0 / 3.0 = 3.0
-- Just (3.0, [])
-- > poland_calcS2 ["1","2","+","3","*","0","/"]
-- 1.0 + 2.0 = 3.0
-- 3.0 * 3.0 = 9.0
-- 9.0 / 0.0 = Nothing

----------------------------------------------------------------
-- unit test
----------------------------------------------------------------

main :: IO ()
main = do
    runTestTT tests_Level0
    print "---------------------"
    runTestTT tests_Level1
    print "---------------------"
    runTestTT tests_Level1_2
    print "---------------------"
    runTestTT tests_Level1_3
    print "---------------------"
    runTestTT tests_Level2
    return ()

tests_Level0 :: Test
tests_Level0 = test [ 
      "poland" ~: do
        let actual = poland_calc ["1","2","*"]
        actual @?= (2.0, [])

        let actual = poland_calc ["1","2","+","3","*","3","/"]
        actual @?= (3.0, [])

        --actual <- poland_calc ["1","2","+","3","*","0","/"]
        --actual @?= Infinity
    ]

tests_Level1 :: Test
tests_Level1 = test [ 
      "polandT" ~: do
        let actual = poland_calcS ["1","2","*"]
        actual @?= Just (2.0, [])

        let actual = poland_calcS ["1","2","+","3","*","3","/"]
        actual @?= Just (3.0, [])

        let actual = poland_calcS ["1","2","+","3","*","0","/"]
        actual @?= Nothing
    ]

tests_Level1_2 :: Test
tests_Level1_2 = test [ 
      "polandMS" ~: do
        actual <- poland_calcMS ["1","2","*"]
        actual @?= (Just 2.0, [])

        actual <- poland_calcMS ["1","2","+","3","*","3","/"]
        actual @?= (Just 3.0, [])

        actual <- poland_calcMS ["1","2","+","3","*","0","/"]
        actual @?= (Nothing, [])
    ]

tests_Level1_3 :: Test
tests_Level1_3 = test [ 
      "polandSM" ~: do
        actual <- poland_calcSM ["1","2","*"]
        actual @?= Just (2.0, [])

        actual <- poland_calcSM ["1","2","+","3","*","3","/"]
        actual @?= Just (3.0, [])

        actual <- poland_calcSM ["1","2","+","3","*","0","/"]
        actual @?= Nothing
    ]

tests_Level2 :: Test
tests_Level2 = test [ 
      "polandT2" ~: do
        actual <- poland_calcS2 ["1","2","*"]
        actual @?= Just (2.0, [])

        actual <- poland_calcS2 ["1","2","+","3","*","0","/"]
        actual @?= Nothing
    ]
