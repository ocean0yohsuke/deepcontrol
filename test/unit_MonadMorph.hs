import Test.HUnit hiding (State)

import DeepControl.Monad.Morph
import Control.Monad.Writer
import Control.Monad.State

-- i.e. :: StateT Int Identity ()
tick    :: State Int ()
tick = modify (+1)

tock                        ::                   StateT Int IO ()
tock = do
    generalize |>| tick     :: (Monad      m) => StateT Int m  ()  -- (|>|) is the level-1 trans-map function, analogous for (|$>)
    lift $ putStrLn "Tock!" :: (MonadTrans t) => t          IO ()

-- λ> runStateT tock 0
-- Tock!
-- ((),1)

-- i.e. :: StateT Int (WriterT [Int] Identity) ()
save    :: StateT Int (Writer  [Int]) ()
save = do
    n <- get
    lift $ tell [n]

program ::                   StateT Int (WriterT [Int] IO) ()
program = replicateM_ 4 $ do
    lift |>| tock
        :: (MonadTrans t) => StateT Int (t             IO) ()
    generalize |>>| save                                        -- (|>>|) is the level-2 trans-map function, analogous for (|$>>)
        :: (Monad      m) => StateT Int (WriterT [Int] m ) ()

-- λ> execWriterT (runStateT program 0)
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
        actual <- execWriterT (runStateT program 0)
        actual @?= [1,2,3,4]
    ]



