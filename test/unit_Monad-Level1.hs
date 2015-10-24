{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Test.HUnit

import DeepControl.Applicative ((|$>))
import DeepControl.Monad ((>-))
import DeepControl.Monad.Trans.Except

import Control.Monad.RWS
import qualified Data.Map as M

------------------------------------------------
-- Data-types

type Name = String          -- variable names

-- Expression
data Exp = Lit Int          -- Literal
         | Var Name         -- Variable
         | Plus Exp Exp     -- (+)
         | Lam Name Exp     -- λ
         | App Exp Exp      -- Application
         deriving (Show, Eq)

-- Value
data Value = IntVal Int          -- Int Value 
           | FunVal Env Name Exp -- Functional Value
           deriving (Show, Eq)

-- Environment
type Env = M.Map Name Value    -- mapping from names to values

------------------------------------------------
-- Monad-Transform

type EvalError = String

newtype Eval a = Eval (RWST Env [String] Int 
                      (ExceptT EvalError
                          IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadError String, MonadReader Env, MonadWriter [String], MonadState Int)

unEval :: Eval a -> (RWST Env [String] Int 
                    (ExceptT EvalError
                        IO) a)
unEval (Eval a) = a

runEval :: Eval a 
           -> Env  -- Reader
           -> Int  -- States
           -> IO (Either EvalError (a, Int, [String])) 
runEval (Eval x) env state = x >- \x -> runRWST x env state
                               >- runExceptT

-- ----------------------------------------------
-- Interpreter

tick :: (Num s, MonadState s m) => m ()
tick = do 
    st <- get
    put (st + 1)

eval :: Exp -> Eval Value
eval (Lit i)   = do 
    tick
    liftIO $ print i
    return $ IntVal i
eval (Var n)   = do 
    tick
    tell [n]
    env <- ask
    case M.lookup n env of
        Nothing  -> throwError $ "unbound variable: " ++ n
        Just val -> return val
eval (Plus e1 e2) = do 
    tick
    e1' <- eval e1
    e2' <- eval e2
    case (e1', e2') of
        (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
        _                      -> throwError "type error in addition"
eval (Lam n e) = do 
    tick
    env <- ask
    return $ FunVal env n e
eval (App e1 e2) = do 
    tick
    val1 <- eval e1
    val2 <- eval e2
    case val1 of
        FunVal env' n body -> local (const (M.insert n val2 env')) $ eval body
        _                  -> throwError "type error in application"

------------------------------------------------
-- Examples

-- 12 + ((\x -> x) (4 + 2))
exp1 :: Exp
exp1 = Lit 12 `Plus` ((Lam "x" (Var "x")) `App` (Lit 4 `Plus` Lit 2))

-- (\x -> (\y -> x + y)) a b
exp2 :: Exp
exp2 = (Lam "x" (Lam "y" ((Var "x") `Plus` (Var "y")))) `App` (Var "a") `App` (Var "b")

-- An environment
env :: Env
env = M.fromList [ ("a", IntVal 1)
                 , ("b", IntVal 2)
                 , ("c", IntVal 3)  
                 , ("d", IntVal 4)
                 ] 

------------------------------------------------
-- Tests

-- > runEval (eval exp1) env 0
-- 12
-- 4
-- 2
-- Right (IntVal 18,8,["x"])

-- > runEval (eval exp2) env 0
-- Right (IntVal 3,9,["a","b","x","y"])

-- > runEval (eval $ Var "x") env 0
-- Left "unbound variable: x"

----------------------------------------------------------------
-- unit test
----------------------------------------------------------------

main :: IO ()
main = do
    runTestTT tests_
    return ()

tests_ :: Test
tests_ = test [ 
      "IO" ~: do
        actual <- runEval (eval exp1) env 0
        actual @?= Right (IntVal 18,8,["x"])

        actual <- runEval (eval exp2) env 0
        actual @?= Right (IntVal 3,9,["a","b","x","y"])
    ]



