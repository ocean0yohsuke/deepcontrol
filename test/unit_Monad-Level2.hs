import Test.HUnit hiding (State)

import DeepControl.Applicative 
import DeepControl.Commutative (cmap)
import DeepControl.Monad ((>-))
import DeepControl.Monad.Morph
import DeepControl.Monad.Trans
import DeepControl.Monad.Trans.Identity

import Control.Monad.Except
import Control.Monad.State
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
    liftT $ guard (x /= 0)
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
-- StateT-Identity2-IO-Maybe Monad

pushSI2 :: a -> StateT [a] (IdentityT2 IO Maybe) a
pushSI2 x = do 
    xs <- get
    put (x:xs)
    return x
popSI2 :: StateT [a] (IdentityT2 IO Maybe) a
popSI2 = do 
    xs <- get
    put (tail xs)
    return (head xs)

polandSI2 :: String -> StateT [Double] (IdentityT2 IO Maybe) Double
polandSI2 "+" = do 
    x <- popSI2
    y <- popSI2
    liftIO $ putStrLn (show y ++" + "++ show x ++" = "++ show (y + x))
    pushSI2 (y + x)
polandSI2 "-" = do 
    x <- popSI2
    y <- popSI2
    liftIO $ putStrLn (show y ++" - "++ show x ++" = "++ show (y - x))
    pushSI2 (y - x)
polandSI2 "*" = do
    x <- popSI2
    y <- popSI2
    liftIO $ putStrLn (show y ++" * "++ show x ++" = "++ show (y * x))
    pushSI2 (y * x)
polandSI2 "/" = do
    x <- popSI2
    y <- popSI2
    liftIO $ putStr (show y ++" / "++ show x ++" = ")
    liftT . liftT2 . (*:) $ guard (x /= 0)
    liftIO $ putStr (show (y / x) ++"\n")
    pushSI2 (y / x)
polandSI2 x = pushSI2 (read x :: Double)

poland_calcSI2 :: [String] -> IO (Maybe (Double, [Double]))
poland_calcSI2 xs = (cmap polandSI2 xs >> popSI2) >- \x -> runStateT x []
                                                  >- runIdentityT2

-----------------------------------------------
-- StateT-IdentityT-MaybeT-IO

popSIM :: StateT [a] (IdentityT (MaybeT IO)) a
popSIM = transfold2 |>| popSI2

polandSIM :: String -> StateT [Double] (IdentityT (MaybeT IO)) Double
polandSIM s = transfold2 |>| polandSI2 s

poland_calcSIM :: [String] -> IO (Maybe (Double, [Double]))
poland_calcSIM xs = (cmap polandSIM xs >> popSIM) >- \x -> runStateT x []
                                                  >- runIdentityT
                                                  >- runMaybeT

-----------------------------------------------
-- StateT-MaybeT-ExceptT-IO Monad

pushSME :: a -> StateT [a] (MaybeT (ExceptT () IO)) a
pushSME x = do 
    xs <- get
    put (x:xs)
    return x
popSME :: StateT [a] (MaybeT (ExceptT () IO)) a
popSME = do 
    xs <- get
    put (tail xs)
    return (head xs)

polandSME :: String -> StateT [Double] (MaybeT (ExceptT () IO)) Double
polandSME "+" = do 
    x <- popSME
    y <- popSME
    liftIO $ putStrLn (show y ++" + "++ show x ++" = "++ show (y + x))
    pushSME (y + x)
polandSME "-" = do 
    x <- popSME
    y <- popSME
    liftIO $ putStrLn (show y ++" - "++ show x ++" = "++ show (y - x))
    pushSME (y - x)
polandSME "*" = do
    x <- popSME
    y <- popSME
    liftIO $ putStrLn (show y ++" * "++ show x ++" = "++ show (y * x))
    pushSME (y * x)
polandSME "/" = do
    x <- popSME
    y <- popSME
    liftIO $ putStr (show y ++" / "++ show x ++" = ")
    guard (x /= 0)
    liftIO $ putStr (show (y / x) ++"\n")
    pushSME (y / x)
polandSME x = pushSME (read x :: Double)

poland_calcSME :: [String] -> IO (Either () (Maybe (Double, [Double])))
poland_calcSME xs = (cmap polandSME xs >> popSME) >- \x -> runStateT x []
                                                  >- runMaybeT
                                                  >- runExceptT

-----------------------------------------------
-- StateT-IdentityT-MaybeT-ExceptT-IO

pushSIME :: a -> StateT [a] (IdentityT (MaybeT (ExceptT () IO))) a
pushSIME x = liftT |>| pushSME x
popSIME :: StateT [a] (IdentityT (MaybeT (ExceptT () IO))) a
popSIME = liftT |>| popSME
polandSIME :: String -> StateT [Double] (IdentityT (MaybeT (ExceptT () IO))) Double
polandSIME x = liftT |>| polandSME x

poland_calcSIME :: [String] -> IO (Either () (Maybe (Double, [Double])))
poland_calcSIME xs = (cmap polandSIME xs >> popSIME) >- \x -> runStateT x []
                                                     >- runIdentityT
                                                     >- runMaybeT
                                                     >- runExceptT

-----------------------------------------------
-- StateT-IdentityT3-IO-Except-Maybe

pushSI3 :: a -> StateT [a] (IdentityT3 IO (Except ()) Maybe) a
pushSI3 x = untransfold3 |>| pushSIME x
popSI3 :: StateT [a] (IdentityT3 IO (Except ()) Maybe) a
popSI3 = untransfold3 |>| popSIME
polandSI3 :: String -> StateT [Double] (IdentityT3 IO (Except ()) Maybe) Double
polandSI3 x = untransfold3 |>| polandSIME x

poland_calcSI3 :: [String] -> IO (Either () (Maybe (Double, [Double])))
poland_calcSI3 xs = (cmap polandSI3 xs >> popSI3) >- \x -> runStateT x []
                                                  >- runIdentityT3
                                                  >- (runExcept|$>)

----------------------------------------------------------------
-- unit test
----------------------------------------------------------------

main :: IO ()
main = do
    runTestTT tests_
    putStrLn "" >> print "---------- S -----------"
    runTestTT tests_S

    putStrLn "" >> print "---------- MS -----------"
    runTestTT tests_MS

    putStrLn "" >> print "---------- SM -----------"
    runTestTT tests_SM

    putStrLn "" >> print "---------- SI2 -----------"
    runTestTT tests_SI2
    putStrLn "" >> print "---------- SIM -----------"
    runTestTT tests_SIM
    putStrLn "" >> print "---------- SIME -----------"
    runTestTT tests_SIME
    putStrLn "" >> print "---------- SI3 -----------"
    runTestTT tests_SI3


    putStrLn "" >> print "---------------------"
    return ()

tests_ :: Test
tests_ = test [ 
      "poland" ~: do
        let actual = poland_calc ["1","2","*"]
        actual @?= (2.0, [])

        let actual = poland_calc ["1","2","+","3","*","3","/"]
        actual @?= (3.0, [])

        --actual <- poland_calc ["1","2","+","3","*","0","/"]
        --actual @?= Infinity
    ]
tests_S :: Test
tests_S = test [ 
      "polandT" ~: do
        let actual = poland_calcS ["1","2","*"]
        actual @?= Just (2.0, [])

        let actual = poland_calcS ["1","2","+","3","*","3","/"]
        actual @?= Just (3.0, [])

        let actual = poland_calcS ["1","2","+","3","*","0","/"]
        actual @?= Nothing
    ]
tests_MS :: Test
tests_MS = test [ 
      "polandMS" ~: do
        actual <- poland_calcMS ["1","2","*"]
        actual @?= (Just 2.0, [])

        actual <- poland_calcMS ["1","2","+","3","*","3","/"]
        actual @?= (Just 3.0, [])

        actual <- poland_calcMS ["1","2","+","3","*","0","/"]
        actual @?= (Nothing, [])
    ]
tests_SM :: Test
tests_SM = test [ 
      "polandSM" ~: do
        actual <- poland_calcSM ["1","2","*"]
        actual @?= Just (2.0, [])

        actual <- poland_calcSM ["1","2","+","3","*","3","/"]
        actual @?= Just (3.0, [])

        actual <- poland_calcSM ["1","2","+","3","*","0","/"]
        actual @?= Nothing
    ]
tests_SI2 :: Test
tests_SI2 = test [ 
      "polandSI2" ~: do
        actual <- poland_calcSI2 ["1","2","*"]
        actual @?= Just (2.0, [])

        actual <- poland_calcSI2 ["1","2","+","3","*","3","/"]
        actual @?= Just (3.0, [])

        actual <- poland_calcSI2 ["1","2","+","3","*","0","/"]
        actual @?= Nothing
    ]
tests_SIM :: Test
tests_SIM = test [ 
      "polandSIM" ~: do
        actual <- poland_calcSIM ["1","2","*"]
        actual @?= Just (2.0, [])

        actual <- poland_calcSIM ["1","2","+","3","*","3","/"]
        actual @?= Just (3.0, [])

        actual <- poland_calcSIM ["1","2","+","3","*","0","/"]
        actual @?= Nothing
    ]
tests_SIME :: Test
tests_SIME = test [ 
      "polandSIME" ~: do
        actual <- poland_calcSIME ["1","2","*"]
        actual @?= Right (Just (2.0, []))

        actual <- poland_calcSIME ["1","2","+","3","*","3","/"]
        actual @?= Right (Just (3.0, []))

        actual <- poland_calcSIME ["1","2","+","3","*","0","/"]
        actual @?= Right Nothing
    ]
tests_SI3 :: Test
tests_SI3 = test [ 
      "polandSI3" ~: do
        actual <- poland_calcSI3 ["1","2","*"]
        actual @?= Right (Just (2.0, []))

        actual <- poland_calcSI3 ["1","2","+","3","*","3","/"]
        actual @?= Right (Just (3.0, []))

        actual <- poland_calcSI3 ["1","2","+","3","*","0","/"]
        actual @?= Right Nothing
    ]


