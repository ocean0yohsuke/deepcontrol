import Test.HUnit

import DeepControl.Applicative
import DeepControl.Traversable (sink)
import DeepControl.Monad ((>-))
import DeepControl.Monad.Morph ((|*|), (|>|))
import DeepControl.Monad.Trans (transfold2, untransfold2)
import DeepControl.Monad.Trans.Identity (Identity(..), IdentityT(..), IdentityT2(..))
import Control.Monad.Reader
import Control.Monad.Trans.Maybe

import System.Timeout (timeout)

type TimeLimit = Int

ackermannTimeLimit :: TimeLimit -> Int -> Int -> 
                      IO (Maybe Int)                      
ackermannTimeLimit timelimit x y = timeout timelimit (ackermannIO x y)
  where
    ackermannIO :: Int -> Int -> IO Int
    ackermannIO 0 n = (.*) $ n + 1
    ackermannIO m n | m > 0 && n == 0 = ackermannIO (m-1) 1
                    | m > 0 && n > 0  = ackermannIO m (n-1) >>= ackermannIO (m-1)
 
ackermann :: Int -> Int -> 
             ReaderT TimeLimit (IdentityT2 IO Maybe) Int  
ackermann x y = do
    timelimit <- ask
    (|*|) . IdentityT2 $ ackermannTimeLimit timelimit x y 

calc_ackermann :: TimeLimit -> Int -> Int -> IO (Maybe Int)
calc_ackermann timelimit x y = ackermann x y >- \r -> runReaderT r timelimit
                                             >- runIdentityT2

-- λ> sink $ calc_ackermann 1000 |$> [0..4] |* 4
-- [Just 5,Just 6,Just 11,Just 125,Nothing]

ackermann' :: Int -> Int -> 
              ReaderT TimeLimit (MaybeT IO) Int                 
ackermann' x y = (transfold2 . runIdentityT2) |>| ackermann x y 

calc_ackermann' :: TimeLimit -> Int -> Int -> IO (Maybe Int)
calc_ackermann' timelimit x y = ackermann' x y >- \r -> runReaderT r timelimit
                                               >- runMaybeT

ackermann'' :: Int -> Int -> 
               ReaderT TimeLimit (IdentityT2 IO Maybe) Int       
ackermann'' x y = (IdentityT2 . untransfold2) |>| ackermann' x y 

calc_ackermann'' :: TimeLimit -> Int -> Int -> IO (Maybe Int)
calc_ackermann'' timelimit x y = ackermann'' x y >- \r -> runReaderT r timelimit
                                                 >- runIdentityT2

----------------------------------------------------------------
-- unit test
----------------------------------------------------------------

main :: IO ()
main = do
    runTestTT tests_1
    runTestTT tests_2
    runTestTT tests_3
    return ()

tests_1 :: Test
tests_1 = test [ 
      "calc" ~: do
        actual <- sink $ calc_ackermann 1000 |$> [0..4] |* 4
        actual @?= [Just 5,Just 6,Just 11,Just 125,Nothing]
    ]

tests_2 :: Test
tests_2 = test [ 
      "calc" ~: do
        actual <- sink $ calc_ackermann' 1000 |$> [0..4] |* 4
        actual @?= [Just 5,Just 6,Just 11,Just 125,Nothing]
    ]

tests_3 :: Test
tests_3 = test [ 
      "calc" ~: do
        actual <- sink $ calc_ackermann'' 1000 |$> [0..4] |* 4
        actual @?= [Just 5,Just 6,Just 11,Just 125,Nothing]
    ]

