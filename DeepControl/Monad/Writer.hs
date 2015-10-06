{-# LANGUAGE MultiParamTypeClasses, 
             FlexibleInstances #-}
module DeepControl.Monad.Writer (
    MonadWriter(..),
    listens, censor,

    Writer(..), execWriter,

    ) where 

import DeepControl.Applicative
import DeepControl.Monad

import Control.Monad.Writer (MonadWriter(..))

listens :: MonadWriter w m => (w -> b) -> m a -> m (a, b)
listens f m = do
    (a, w) <- listen m
    return (a, f w)

censor :: MonadWriter w m => (w -> w) -> m a -> m a
censor f m = pass $ do
    a <- m
    return (a, f)

----------------------------------------------------------------------
-- Writer

newtype Writer w a = Writer { runWriter :: (a, w) }

instance Functor (Writer w) where
    fmap f v = Writer $ (\(a, w) -> (f a, w)) $ (runWriter v)
instance (Monoid w) => Applicative (Writer w) where
    pure a = Writer $ (a, mempty)
    (<*>) = \(Writer (f, w)) (Writer (a, w')) ->
        Writer (f a, w' `mappend` w)

instance (Monoid w) => Monad (Writer w) where
    return   = (*:)
    mv >>= f = 
        mv >- \(Writer (a, w)) -> 
        (\(Writer (b, w')) -> Writer (b, w `mappend` w')) $ f a
instance (Monoid w) => Monad2 (Writer w) where
    mmv >>== f = 
        mmv >>= \(Writer (a, w)) -> 
        (\(Writer (b, w')) -> Writer (b, w `mappend` w')) |$> f a
instance (Monoid w) => Monad3 (Writer w) where
    mmv >>>== f = 
        mmv >>== \(Writer (a, w)) -> 
        (\(Writer (b, w')) -> Writer (b, w `mappend` w')) |$>> f a
instance (Monoid w) => Monad4 (Writer w) where
    mmv >>>>== f = 
        mmv >>>== \(Writer (a, w)) -> 
        (\(Writer (b, w')) -> Writer (b, w `mappend` w')) |$>>> f a
instance (Monoid w) => Monad5 (Writer w) where
    mmv >>>>>== f = 
        mmv >>>>== \(Writer (a, w)) -> 
        (\(Writer (b, w')) -> Writer (b, w `mappend` w')) |$>>>> f a

instance (Monoid w) => MonadWriter w (Writer w) where
    writer   = Writer
    tell w   = writer ((), w)
    listen m = Writer $ 
        runWriter m >- \(a, w) ->
        ((a, w), w) 
    pass m   = Writer $ 
        runWriter m >- \((a, f), w) ->
        (a, f w)

execWriter :: Writer w a -> w
execWriter m =
    runWriter m >- \(_, w) ->
    w


