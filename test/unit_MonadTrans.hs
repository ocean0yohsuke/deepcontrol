import Test.HUnit hiding (State)

import DeepControl.Applicative ((|$>))
import DeepControl.Monad (Monad2)
import DeepControl.Monad.Trans (lift, (|*|), (|-*|), (|*-|))
import DeepControl.Monad.Trans.State
import DeepControl.Monad.Trans.Writer

tick :: State Int ()
tick = modify (+1)

tock                        ::                   StateT Int IO ()
tock = do
    (|*|) tick              :: (Monad      m) => StateT Int m  ()
    lift $ putStrLn "Tock!" :: (MonadTrans t) => t          IO ()

-- λ> runStateT tock 0
-- Tock!
-- ((),1)

save    :: StateT Int (Writer [Int]) ()
save = do
    n <- get
    lift $ tell [n]

program ::                   StateT2 Int IO (Writer [Int]) ()
program = replicateM_ 4 $ do
    (|-*|) tock
        :: (Monad2     m) => StateT2 Int IO m              ()
    (|*-|) save
        :: (Monad      m) => StateT2 Int m  (Writer [Int]) ()

-- λ> execWriter |$> runStateT2 program 0
-- Tock!
-- Tock!
-- Tock!
-- Tock!
-- [1,2,3,4]

----------------------------------------------------------------
-- unit test
----------------------------------------------------------------

main :: IO ()
main = do
    runTestTT tests_
    return ()

tests_ :: Test
tests_ = test [ 
      "tock" ~: do
        actual <- runStateT tock 0
        actual @?= ((),1)

    , "program" ~: do
        actual <- execWriter |$> runStateT2 program 0
        actual @?= [1,2,3,4]
    ]



