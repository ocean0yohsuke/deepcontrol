{-|
Module      : DeepControl.MonadTrans
Description : Enable deep level Monad-Transform programming.
Copyright   : (c) Andy Gill 2001,
              (c) Oregon Graduate Institute of Science and Technology, 2001,
              (C) 2015 KONISHI Yohsuke 
License     : BSD-style (see the file LICENSE)
Maintainer  : ocean0yohsuke@gmail.com
Stability   : experimental
Portability : ---

This module enables you to program in Monad-Transformer style for more __deeper__ level than the usual @Control.Monad.Trans@ module expresses.
You would realize exactly what __/more deeper level/__ means by reading the example codes, which are attached on the page bottom.
Note: all the MonadTransx instances for Level-4 and Level-5 haven't been written yet.
-}
module DeepControl.MonadTrans (
    -- * MonadIO
    MonadIO(..),

    -- * MonadTrans
    MonadTrans(..), 
    MonadTrans2(..),
    MonadTrans3(..),
    MonadTrans4(..),
    MonadTrans5(..),

    -- * Level-1 example
    -- $Example_Level1

    -- * Level-2 example
    -- $Example_Level2

) where

import DeepControl.Monad

import Control.Monad.IO.Class

----------------------------------------------------------------------
-- Level-1

class  MonadTrans t  where
    -- | Alias for @'Control.Monad.Trans.Class.lift'@.
    trans :: (Monad m) => m a -> t m a

----------------------------------------------------------------------
-- Level-2

class  MonadTrans2 t  where
    trans2 :: (Monad m1, Monad2 m2) => m1 (m2 a) -> t m1 m2 a

----------------------------------------------------------------------
-- Level-3

class  MonadTrans3 t  where
    trans3 :: (Monad m1, Monad2 m2, Monad3 m3) => m1 (m2 (m3 a)) -> t m1 m2 m3 a

----------------------------------------------------------------------
-- Level-4

class  MonadTrans4 t  where
    trans4 :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m1 (m2 (m3 (m4 a))) -> t m1 m2 m3 m4 a

----------------------------------------------------------------------
-- Level-5

class  MonadTrans5 t  where
    trans5 :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m1 (m2 (m3 (m4 (m5 a)))) -> t m1 m2 m3 m4 m5 a

----------------------------------------------------------------------
-- Examples

{- $Example_Level1
Here is a monad transformer example how to implement a tiny interpreter with RWST-ExceptT-IO monad, a level-1 monad-transformation.

Please turn on three pragmas GeneralizedNewtypeDeriving, FlexibleInstances and MultiParamTypeClasses on this example.

>import DeepControl.Applicative
>import DeepControl.Monad
>import DeepControl.MonadTrans
>import DeepControl.Monad.Except
>import DeepControl.Monad.RWS
>
>import qualified Data.Map as M
>
>-- ----------------------------------------------
>-- Data-types
>
>type Name = String          -- variable names
>
>-- Expression
>data Exp = Lit Int          -- Literal
>         | Var Name         -- Variable
>         | Plus Exp Exp     -- (+)
>         | Lam Name Exp     -- λ
>         | App Exp Exp      -- Application
>         deriving (Show)
>
>-- Value
>data Value = IntVal Int          -- Int Value 
>           | FunVal Env Name Exp -- Functional Value
>           deriving (Show)
>
>-- Environment
>type Env = M.Map Name Value    -- mapping from names to values
>
>-- ----------------------------------------------
>-- Monad-Transform
>
>type EvalError = String
>instance Error EvalError where
>    strMsg x = x
> 
>newtype Eval a = Eval (RWST Env [String] Int 
>                      (ExceptT EvalError
>                          IO) a)
>  deriving (Functor, Applicative, Monad, MonadIO)
>
>unEval :: Eval a -> (RWST Env [String] Int 
>                    (ExceptT EvalError
>                        IO) a)
>unEval (Eval a) = a
>
>runEval :: Eval a 
>           -> Env  -- Reader
>           -> Int  -- States
>           -> IO (Either EvalError (a, Int, [String])) 
>runEval (Eval x) env state = x >- \x -> runRWST x env state
>                               >- runExceptT
>
>instance MonadError EvalError Eval where
>    throwError = Eval . trans . throwError
>    catchError x h = 
>        let x' = unEval x
>        in Eval $ (liftCatch catchError x') (\e -> unEval (h e))
>instance MonadReader Env Eval where
>    ask     = Eval $ ask
>    local x = Eval . (local x) . unEval
>instance MonadWriter [String] Eval where
>    writer   = Eval . writer
>    listen m = Eval $ (listen (unEval m)) 
>    pass m   = Eval $ (pass (unEval m)) 
>instance MonadState Int Eval where
>    get   = Eval $ get
>    put   = Eval . put
>    state = Eval . state
>
>-- ----------------------------------------------
>-- Interpreter
>
>tick :: (Num s, MonadState s m) => m ()
>tick = do 
>    st <- get
>    put (st + 1)
>
>eval :: Exp -> Eval Value
>eval (Lit i)   = do 
>    tick
>    liftIO $ print i
>    return $ IntVal i
>eval (Var n)   = do 
>    tick
>    tell [n]
>    env <- ask
>    case M.lookup n env of
>        Nothing  -> throwError $ "unbound variable: " ++ n
>        Just val -> return val
>eval (Plus e1 e2) = do 
>    tick
>    e1' <- eval e1
>    e2' <- eval e2
>    case (e1', e2') of
>        (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
>        _                      -> throwError "type error in addition"
>eval (Lam n e) = do 
>    tick
>    env <- ask
>    return $ FunVal env n e
>eval (App e1 e2) = do 
>    tick
>    val1 <- eval e1
>    val2 <- eval e2
>    case val1 of
>        FunVal env' n body -> local (const (M.insert n val2 env')) $ eval body
>        _                  -> throwError "type error in application"
>
>-- ----------------------------------------------
>-- Examples
>
>-- 12 + ((\x -> x) (4 + 2))
>exp1 :: Exp
>exp1 = Lit 12 `Plus` ((Lam "x" (Var "x")) `App` (Lit 4 `Plus` Lit 2))
>
>-- (\x -> (\y -> x + y)) a b
>exp2 :: Exp
>exp2 = (Lam "x" (Lam "y" ((Var "x") `Plus` (Var "y")))) `App` (Var "a") `App` (Var "b")
>
>-- An environment
>env :: Env
>env = M.fromList [ ("a", IntVal 1)
>                 , ("b", IntVal 2)
>                 , ("c", IntVal 3)  
>                 , ("d", IntVal 4)
>                 ] 
>
>-- ----------------------------------------------
>-- Tests
>--
>-- > runEval (eval exp1) env 0
>-- 12
>-- 4
>-- 2
>-- Right (IntVal 18,8,["x"])
>
>-- > runEval (eval exp2) env 0
>-- Right (IntVal 3,9,["a","b","x","y"])
>
>-- > runEval (eval $ Var "x") env 0
>-- Left "unbound variable: x"
-}

{- $Example_Level2
Here is a monad transformer example how to implement Polish Notation with StateT2-IO-Maybe monad, a level-2 monad-transformation.

>import DeepControl.Applicative
>import DeepControl.Commutative (cmap)
>import DeepControl.Monad
>import DeepControl.Monad.State
>import DeepControl.MonadTrans
>
>-----------------------------------------------
>-- State
>
>push :: a -> State [a] a
>push x = do 
>    xs <- get
>    put (x:xs)
>    return x
>
>pop :: State [a] a
>pop = do 
>    xs <- get
>    put (tail xs)
>    return (head xs)
>
>-- > runState (push 1 >> push 2 >> push 3) []
>-- (3,[3,2,1])
>-- > runState (push 1 >> push 2 >> push 3 >> pop >> pop) []
>-- (2,[1])
>
>poland :: String -> State [Double] Double
>poland "+" = do 
>    x <- pop
>    y <- pop
>    push (y + x)
>poland "-" = do 
>    x <- pop
>    y <- pop
>    push (y - x)
>poland "*" = do 
>    x <- pop
>    y <- pop
>    push (y * x)
>poland "/" = do
>    x <- pop
>    y <- pop
>    push (y / x)
>poland x = push (read x :: Double)
>
>poland_calc :: [String] -> Double
>poland_calc xs = evalState (cmap poland xs >> pop) []
>
>-- > poland_calc ["1","2","*"]
>-- 2.0
>-- > poland_calc ["1","2","-"]
>-- -1.0
>-- > poland_calc ["1","2","+","3","*"]
>-- 9.0
>-- > poland_calc ["1","2","+","3","*","3","/"]
>-- 3.0
>-- > poland_calc ["1","2","+","3","*","0","/"]
>-- Infinity
>
>-----------------------------------------------
>-- StateT-Maybe
>
>pushT :: a -> StateT [a] Maybe a
>pushT x = do 
>    xs <- get
>    put (x:xs)
>    return x
>
>popT :: StateT [a] Maybe a
>popT = do 
>    xs <- get
>    put (tail xs)
>    return (head xs)
>
>-- > runStateT (pushT 1 >> pushT 2 >> pushT 3) []
>-- Just (3,[3,2,1])
>-- > runStateT (pushT 1 >> pushT 2 >> pushT 3 >> popT >> popT) []
>-- Just (2,[1])
>
>polandT :: String -> StateT [Double] Maybe Double
>polandT "+" = do 
>    x <- popT
>    y <- popT
>    pushT (y + x)
>polandT "-" = do 
>    x <- popT
>    y <- popT
>    pushT (y - x)
>polandT "*" = do 
>    x <- popT
>    y <- popT
>    pushT (y * x)
>polandT "/" = do
>    x <- popT
>    y <- popT
>    trans $ guard (x /= 0)
>    pushT (y / x)
>polandT x = pushT (read x :: Double)
>
>poland_calcT :: [String] -> Maybe Double
>poland_calcT xs = evalStateT (cmap polandT xs >> popT) []
>
>-- > poland_calcT ["1","2","*"]
>-- Just 2.0
>-- > poland_calcT ["1","2","-"]
>-- Just (-1.0)
>-- > poland_calcT ["1","2","+","3","*"]
>-- Just 9.0
>-- > poland_calcT ["1","2","+","3","*","3","/"]
>-- Just 3.0
>-- > poland_calcT ["1","2","+","3","*","0","/"]
>-- Nothing
>
>-----------------------------------------------
>-- StateT2-IO-Maybe
>
>pushT2 :: a -> StateT2 [a] IO Maybe a
>pushT2 x = do 
>    xs <- get
>    put (x:xs)
>    return x
>popT2 :: StateT2 [a] IO Maybe a
>popT2 = do 
>    xs <- get
>    put (tail xs)
>    return (head xs)
>
>polandT2 :: String -> StateT2 [Double] IO Maybe Double
>polandT2 "+" = do 
>    x <- popT2
>    y <- popT2
>    liftIO $ putStrLn (show y ++" + "++ show x ++" = "++ show (y + x))
>    pushT2 (y + x)
>polandT2 "-" = do 
>    x <- popT2
>    y <- popT2
>    liftIO $ putStrLn (show y ++" - "++ show x ++" = "++ show (y - x))
>    pushT2 (y - x)
>polandT2 "*" = do
>    x <- popT2
>    y <- popT2
>    liftIO $ putStrLn (show y ++" * "++ show x ++" = "++ show (y * x))
>    pushT2 (y * x)
>polandT2 "/" = do
>    x <- popT2
>    y <- popT2
>    liftIO $ putStr (show y ++" / "++ show x ++" = ")
>    trans2.(*:) $ guard (x /= 0)
>    liftIO $ putStr (show (y / x) ++"\n")
>    pushT2 (y / x)
>polandT2 x = pushT2 (read x :: Double)
>
>poland_calcT2 :: [String] -> IO (Maybe Double)
>poland_calcT2 xs = evalStateT2 (cmap polandT2 xs >> popT2) []
>
>-- > poland_calcT2 ["1","2","*"]
>-- 1.0 * 2.0 = 2.0
>-- Just 2.0
>-- > poland_calcT2 ["1","2","+","3","*"]
>-- 1.0 + 2.0 = 3.0
>-- 3.0 * 3.0 = 9.0
>-- Just 9.0
>-- > poland_calcT2 ["1","2","+","3","*","3","/"]
>-- 1.0 + 2.0 = 3.0
>-- 3.0 * 3.0 = 9.0
>-- 9.0 / 3.0 = 3.0
>-- Just 3.0
>-- > poland_calcT2 ["1","2","+","3","*","0","/"]
>-- 1.0 + 2.0 = 3.0
>-- 3.0 * 3.0 = 9.0
>-- 9.0 / 0.0 = Nothing
-}
