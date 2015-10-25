import Test.HUnit hiding (State)

import DeepControl.Applicative ((|$>>), (*-*), (*:), (**:), (***:))
import DeepControl.Monad ((>>), (>>>==), (>--~), (-->~))
import Control.Monad.Writer

factorial :: Int ->
             IO (Maybe (Writer [Int] Int))            -- IO-Maybe-Writer monad
factorial n | n < 0  = (*-*) Nothing                  -- (*-*) is a level-3 cover function
            | n == 0 = (**:) $ tell [0] >> (*:) 1
            | n > 0  = factorial (n-1) >>>== \v ->    -- (>>>==) is the level-3 bind function, analogous for (>>=)
                       print v >--~                   -- (>--~) is a level-3 bind-cover function, analogous for (>>)
                       tell [v] -->~                  -- (-->~) is a level-3 bind-cover function too, analogous for (>>)
                       (***:) (n * v)

-- > runWriter |$>> factorial 5
-- 1
-- 1
-- 2
-- 6
-- 24
-- Just (120,[0,1,1,2,6,24])

main :: IO ()
main = do
    runTestTT tests_
    return ()

tests_ :: Test
tests_ = test [ 
      "factorial" ~: do
        actual <- runWriter |$>> factorial 5
        actual @?= Just (120,[0,1,1,2,6,24])

    ]

