import Test.HUnit hiding (State)

import DeepControl.Applicative ((|$>))
import DeepControl.Monad (Monad2)
import DeepControl.Monad.Morph ((|>|))
import DeepControl.Monad.Trans (lift, (|*|), (|-*|), (|*-|))
import DeepControl.Monad.Trans.Writer
import DeepControl.Monad.Trans.Identity
import DeepControl.Monad.Trans.State

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

program ::               StateT Int (IdentityT2 IO (Writer [Int])) ()
program = replicateM_ 4 $ do
    ((|-*|).lift) |>| tock
        :: (Monad2 m) => StateT Int (IdentityT2 IO m             ) ()
    ((|*-|).lift) |>| save
        :: (Monad  m) => StateT Int (IdentityT2 m  (Writer [Int])) ()

-- λ> execWriter |$> runIdentityT2 (runStateT program 0)
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
        actual <- execWriter |$> runIdentityT2 (runStateT program 0)
        actual @?= [1,2,3,4]
    ]



