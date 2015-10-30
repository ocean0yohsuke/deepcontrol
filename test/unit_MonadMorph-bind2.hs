import Test.HUnit hiding (State)

import DeepControl.Applicative
import DeepControl.Monad.Morph 
import DeepControl.Monad.Trans.Except
import DeepControl.Monad.Trans.Identity

import Control.Monad.Trans.Maybe

-----------------------------------------------
-- Level-1 

a :: MaybeT Identity a
a = (|*|) $ (.*) undefined

b :: MaybeT Identity a
b = (|*|) $ (.*) undefined

f :: Identity a -> Identity a1 -> IO a2
f x y = (.*) $ undefined

program :: MaybeT IO a
program = 
    a |>= \x ->
    b |>= \y -> 
    (|*|) (f x y)

calc_program :: IO (Maybe a)
calc_program = runMaybeT program

-- > calc_program

------------------------------------------------------
--

main :: IO ()
main = do
    --runTestTT tests_
    return ()

{-
tests_ :: Test
tests_ = test [ 
      "1" ~: do
        actual <- calc_program
        actual @?= Just undefined
    ]
-}

