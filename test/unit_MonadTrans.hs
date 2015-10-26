import Test.HUnit hiding (State)

import DeepControl.Applicative ((|$>))
import DeepControl.Commutative (Commutative)
import DeepControl.Monad (Monad)
import DeepControl.Monad.Morph (generalize, (|>|))
import DeepControl.Monad.Trans (liftT)
import DeepControl.Monad.Trans.Identity (IdentityT2(..), (|-*|), (|*-|))
import Control.Monad.Writer
import Control.Monad.State

tick :: State Int ()
tick = modify (+1)

tock                         ::                   StateT Int IO ()
tock = do
    generalize |>| tick      :: (Monad      m) => StateT Int m  ()  -- (|>|) is the level-1 trans-map function, analogous for (|$>)
    liftT $ putStrLn "Tock!" :: (MonadTrans t) => t          IO ()  -- 'liftT' is the level-1 trans-lift function, alias to 'lift'

-- λ> runStateT tock 0
-- Tock!
-- ((),1)

save :: StateT Int (Writer [Int]) ()
save = do
    n <- get
    liftT $ tell [n]

program ::                             StateT Int (IdentityT2 IO (Writer [Int])) () -- StateT-IdentityT2-IO-Writer monad, a level-2 monad-transform
program = replicateM_ 4 $ do
    ((|-*|).liftT) |>| tock                                                         -- (|-*|) is a level-2 trans-cover function, analogous for (-*)
        :: (Monad m, Commutative m) => StateT Int (IdentityT2 IO m             ) ()
    ((|*-|).liftT) |>| save                                                         -- (|*-|) is a level-2 trans-cover function, analogous for (*:)
        :: (Monad m               ) => StateT Int (IdentityT2 m  (Writer [Int])) ()

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

