import Test.HUnit

import DeepControl.Applicative
import DeepControl.Commutative (commute)
import DeepControl.Monad ((>-))
import DeepControl.Monad.Trans (trans2)
import DeepControl.Monad.Trans.Reader

import System.Timeout (timeout)

type TimeLimit = Int

ackermannTimeLimit :: TimeLimit -> Int -> Int
                      -> IO (Maybe Int)             -- IO-Maybe Monad
ackermannTimeLimit timelimit x y = timeout timelimit (ackermannIO x y)
  where
    ackermannIO :: Int -> Int -> IO Int
    ackermannIO 0 n = (*:) $ n + 1
    ackermannIO m n | m > 0 && n == 0 = ackermannIO (m-1) 1
                    | m > 0 && n > 0  = ackermannIO m (n-1) >>= ackermannIO (m-1)
 
ackermannR :: Int -> Int 
              -> ReaderT2 TimeLimit IO Maybe Int    -- ReaderT2-IO-Maybe Monad
ackermannR x y = do
    timelimit <- ask
    trans2 $ ackermannTimeLimit timelimit x y       -- transform IO-Maybe function to ReaderT2-IO-Maybe function

calc_ackermann :: TimeLimit -> Int -> Int -> IO (Maybe Int)
calc_ackermann timelimit x y = ackermannR x y >- \r -> runReaderT2 r timelimit

-- λ> commute $ calc_ackermann 1000 |$> [0..4] |* 4
-- [Just 5,Just 6,Just 11,Just 125,Nothing]

----------------------------------------------------------------
-- unit test
----------------------------------------------------------------

main :: IO ()
main = do
    runTestTT tests_1
    return ()

tests_1 :: Test
tests_1 = test [ 
      "calc" ~: do
        actual <- commute $ calc_ackermann 1000 |$> [0..4] |* 4
        actual @?= [Just 5,Just 6,Just 11,Just 125,Nothing]
    ]



