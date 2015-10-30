import Test.HUnit hiding (State)

import DeepControl.Monad.Morph ((|>=), (|>>=), (|*|), (|-*|))
import DeepControl.Monad.Trans.Except

import Control.Monad.Trans.Maybe
import Control.Exception (IOException, try)

-----------------------------------------------
-- Level-1 

check :: IO a -> 
         ExceptT IOException IO a          -- ExceptT-IO monad
check io = ExceptT $ (try io)

viewFile :: IO ()                          -- IO monad
viewFile = do
    str <- readFile "test.txt"        
    putStr str

program :: ExceptT IOException IO ()       -- ExceptT-IO monad
program = (|*|) viewFile |>= check         -- (|*|) is the level-1 trans-cover function, alias to 'lift', analogous to (.*)
                                           -- (|>=) is the level-1 trans-bind function, analogous to (>>=)

calc_program :: IO (Either IOException ())
calc_program = runExceptT $ program        

-- > calc_program
-- Left test.txt: openFile: does not exist (No such file or directory)

-----------------------------------------------
-- Level-2

viewFile2 :: String -> 
             MaybeT IO ()                        -- MaybeT-IO monad
viewFile2 filename = do
    guard (filename /= "")
    str <- (|*|) $ readFile filename       
    (|*|) $ putStr str

program2 :: String -> 
            (ExceptT IOException (MaybeT IO)) () -- ExceptT-MaybeT-IO monad
program2 filename = 
    (|*|) (viewFile2 filename) |>>= \x ->        -- (|>>=) is the level-2 trans-bind function, analogous to (>>=)
    (|-*|) $ check x                             -- (|-*|) is a level-2 trans-cover function, analogous to (-*)

calc_program2 :: String -> IO (Maybe (Either IOException ())) 
calc_program2 filename = runMaybeT . runExceptT $ program2 filename

-- > calc_program2 "test.txt"
-- Just (Left test.txt: openFile: does not exist (No such file or directory))
-- > calc_program2 ""
-- Nothing

------------------------------------------------------
--

main :: IO ()
main = do
    runTestTT tests_
    runTestTT tests2_
    return ()

tests_ :: Test
tests_ = test [ 
      "1" ~: do
        actual <- calc_program 
        print actual
        return ()
    ]

tests2_ :: Test
tests2_ = test [ 
      "2" ~: do
        actual <- calc_program2 "test.txt"
        print actual
        return ()
        actual <- calc_program2 ""
        actual @?= Nothing

    ]

