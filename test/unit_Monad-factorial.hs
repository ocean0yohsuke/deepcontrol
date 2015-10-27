import Test.HUnit hiding (State)

import DeepControl.Applicative ((|$>), (.*), (.**))
import DeepControl.Monad ((>>), (>>==), (->~))
import Control.Monad.Writer

factorial :: Int ->
             Maybe (Writer [Int] Int)               -- Maybe-Writer monad
factorial n | n < 0  = Nothing
            | n == 0 = (.*) $ tell [0] >> (.*) 1
            | n > 0  = factorial (n-1) >>== \v ->   
                       tell [v] ->~                 -- (->~) is a level-2 bind-cover function, analogous for (>>)
                       (.**) (n * v)

-- > runWriter |$> factorial 5
-- Just (120,[0,1,1,2,6,24])

-- > factorial (-1)
-- Nothing

main :: IO ()
main = do
    runTestTT tests_
    return ()

tests_ :: Test
tests_ = test [ 
      "factorial" ~: do
        let actual = runWriter |$> factorial 5
        actual @?= Just (120,[0,1,1,2,6,24])

        let actual = factorial (-1)
        actual @?= Nothing

    ]

