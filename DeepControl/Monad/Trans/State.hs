{-|
Module      : DeepControl.Monad.Trans.State
Description : 
Copyright   : (c) Andy Gill 2001,
              (c) Oregon Graduate Institute of Science and Technology, 2001,
              (C) 2015 KONISHI Yohsuke,
License     : BSD-style (see the file LICENSE)
Maintainer  : ocean0yohsuke@gmail.com
Stability   : experimental
Portability : ---

This module extended State Monad in mtl(monad-transformer-library).
-}
{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances,
             UndecidableInstances #-}
module DeepControl.Monad.Trans.State (
    module Control.Monad.State,

    -- * Level-2
    StateT2(..), evalStateT2, execStateT2, mapStateT2, withStateT2, 
    -- * Level-3
    StateT3(..), evalStateT3, execStateT3, mapStateT3, withStateT3, 

    ) where 

import DeepControl.Applicative
import DeepControl.Monad
import DeepControl.Monad.Trans

import Control.Monad.State 
import Control.Monad.Signatures

----------------------------------------------------------------------
-- Level-2

newtype StateT2 s m1 m2 a = StateT2 { runStateT2 :: (s -> m1 (m2 (a,s))) }

instance (Functor m1, Functor m2) => Functor (StateT2 s m1 m2) where
    fmap f v = StateT2 $ \s ->
        (\(a, s') -> (f a, s')) |$>> runStateT2 v s
instance (Monad m1, Monad2 m2) => Applicative (StateT2 s m1 m2) where
    pure a = StateT2 $ \s -> (**:) (a,s)
    (<*>)  = ap
instance (Monad m1, Monad2 m2) => Monad (StateT2 s m1 m2) where
    return = pure
    (StateT2 v) >>= f = 
        StateT2 $ \s -> 
            v s >>== \(a, s') ->
            runStateT2 (f a) s'
instance (Monad m1, Monad2 m2) => MonadState s (StateT2 s m1 m2) where
    get   = StateT2 $ \s -> (**:) (s, s)
    put s = StateT2 $ \_ -> (**:) ((), s)

instance MonadTrans2 (StateT2 s) where
    trans2 m = StateT2 $ \s -> 
        m >>== \a ->
        (**:) (a, s)
instance (MonadIO m1, Monad m1, Monad2 m2) => MonadIO (StateT2 s m1 m2) where
    liftIO = trans2 . (-*) . liftIO

evalStateT2 :: (Monad m1, Monad2 m2) => StateT2 s m1 m2 a -> s -> m1 (m2 a)
evalStateT2 m s = 
    runStateT2 m s >>== \(a, _) ->
    (**:) a
execStateT2 :: (Monad m1, Monad2 m2) => StateT2 s m1 m2 a -> s -> m1 (m2 s)
execStateT2 m s = 
    runStateT2 m s >>== \(_, s') ->
    (**:) s'

mapStateT2 :: (m1 (m2 (a, s)) -> n1 (n2 (b, s))) -> StateT2 s m1 m2 a -> StateT2 s n1 n2 b
mapStateT2 f m = StateT2 $ f . runStateT2 m
withStateT2 :: (s -> s) -> StateT2 s m1 m2 a -> StateT2 s m1 m2 a
withStateT2 f m = StateT2 $ runStateT2 m . f

----------------------------------------------------------------------
-- Level-3

newtype StateT3 s m1 m2 m3 a = StateT3 { runStateT3 :: (s -> m1 (m2 (m3 (a,s)))) }

instance (Functor m1, Functor m2, Functor m3) => Functor (StateT3 s m1 m2 m3) where
    fmap f v = StateT3 $ \s ->
        (\(a, s') -> (f a, s')) |$>>> runStateT3 v s
instance (Monad m1, Monad2 m2, Monad3 m3) => Applicative (StateT3 s m1 m2 m3) where
    pure a = StateT3 $ \s -> (***:) (a,s)
    (<*>)  = ap
instance (Monad m1, Monad2 m2, Monad3 m3) => Monad (StateT3 s m1 m2 m3) where
    return = pure
    (StateT3 v) >>= f = 
        StateT3 $ \s -> 
            v s >>>== \(a, s') ->
            runStateT3 (f a) s'
instance (Monad m1, Monad2 m2, Monad3 m3) => MonadState s (StateT3 s m1 m2 m3) where
    get   = StateT3 $ \s -> (***:) (s, s)
    put s = StateT3 $ \_ -> (***:) ((), s)

instance MonadTrans3 (StateT3 s) where
    trans3 m = StateT3 $ \s -> 
        m >>>== \a ->
        (***:) (a, s)
instance (MonadIO m1, Monad m1, Monad2 m2, Monad3 m3) => MonadIO (StateT3 s m1 m2 m3) where
    liftIO = trans3 . (-**) . liftIO

evalStateT3 :: (Monad m1, Monad2 m2, Monad3 m3) => StateT3 s m1 m2 m3 a -> s -> m1 (m2 (m3 a))
evalStateT3 m s = 
    runStateT3 m s >>>== \(a, _) ->
    (***:) a
execStateT3 :: (Monad m1, Monad2 m2, Monad3 m3) => StateT3 s m1 m2 m3 a -> s -> m1 (m2 (m3 s))
execStateT3 m s = 
    runStateT3 m s >>>== \(_, s') ->
    (***:) s'

mapStateT3 :: (m1 (m2 (m3 (a, s))) -> n1 (n2 (n3 (b, s)))) -> StateT3 s m1 m2 m3 a -> StateT3 s n1 n2 n3 b
mapStateT3 f m = StateT3 $ f . runStateT3 m
withStateT3 :: (s -> s) -> StateT3 s m1 m2 m3 a -> StateT3 s m1 m2 m3 a
withStateT3 f m = StateT3 $ runStateT3 m . f






