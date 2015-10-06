{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances,
             UndecidableInstances #-}
module DeepControl.Monad.State (
    MonadState(..),
    modify, gets,

    State(..), evalState, execState, 

    ) where 

import DeepControl.Applicative
import DeepControl.Monad

import Control.Monad.State (MonadState(..))

modify :: MonadState s m => (s -> s) -> m ()
modify f = state $ \s -> ((), f s)

gets :: MonadState s m => (s -> a) -> m a
gets f = state $ \s -> (f s, s)

----------------------------------------------------------------------
-- State

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap f v = State $ \s ->
        (\(a, s') -> (f a, s')) $ runState v s
instance Applicative (State s) where
    pure a = State $ \s -> (a,s) 
    (<*>) = ap
instance Monad (State s) where  
    return          = (*:)
    (State v) >>= f = 
        State $ \s -> 
            v s >- \(a, s') ->
            runState (f a) s'

instance MonadState s (State s) where
    get   = State $ \s -> (s, s)
    put s = State $ \_ -> ((), s)

evalState :: State s a -> s -> a
evalState m s = 
    let (a, _) = runState m s
    in a
execState :: State s a -> s -> s
execState m s = 
    let (_, s') = runState m s
    in s'


