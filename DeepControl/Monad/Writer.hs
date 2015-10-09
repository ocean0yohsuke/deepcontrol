{-|
Module      : DeepControl.Monad.State
Description : 
Copyright   : (c) Andy Gill 2001,
              (c) Oregon Graduate Institute of Science and Technology, 2001,
              (C) 2015 KONISHI Yohsuke,
License     : BSD-style (see the file LICENSE)
Maintainer  : ocean0yohsuke@gmail.com
Stability   : experimental
Portability : ---

This module is just a concise mimic for Reader Monad in mtl(monad-transformer-library).
The qualifier "concise" means that this module doesn't make no attempt to transform functions of any kind of Monad automatically.
So when making some new data type of WriterT, you have to manually define involved Monad instances, 
for example `DeepControl.Monad.MonadError`, 
by making use of the transformation functions such as `trans`, `trans2`, etc.
Admittedly it is tedious though, you can deeply understand monad-transformation mechanism instead.
-}
{-# LANGUAGE MultiParamTypeClasses, 
             FlexibleInstances #-}
module DeepControl.Monad.Writer (
    MonadWriter(..),
    listens, censor,

    -- * Level-0
    Writer(..), execWriter, mapWriter,
    -- * Level-1
    WriterT(..), execWriterT, mapWriterT, liftCatch,
    -- * Level-2
    WriterT2(..), execWriterT2, mapWriterT2,
    -- * Level-3
    WriterT3(..), execWriterT3, mapWriterT3,

    ) where 

import DeepControl.Applicative
import DeepControl.Monad
import DeepControl.MonadTrans

import Control.Monad.Writer (MonadWriter(..))
import Control.Monad.Signatures
import Data.Monoid

listens :: MonadWriter w m => (w -> b) -> m a -> m (a, b)
listens f m = do
    (a, w) <- listen m
    return (a, f w)

censor :: MonadWriter w m => (w -> w) -> m a -> m a
censor f m = pass $ do
    a <- m
    return (a, f)

----------------------------------------------------------------------
-- Level-0

newtype Writer w a = Writer { runWriter :: (a, w) }

instance Functor (Writer w) where
    fmap f v = Writer $ (\(a, w) -> (f a, w)) $ (runWriter v)
instance (Monoid w) => Applicative (Writer w) where
    pure a = Writer $ (a, mempty)
    (<*>) = \(Writer (f, w)) (Writer (a, w')) ->
        Writer (f a, w <> w')

instance (Monoid w) => Monad (Writer w) where
    return = pure
    mv >>= f = 
        mv >- \(Writer (a, w)) -> 
        (\(Writer (b, w')) -> Writer (b, w <> w')) $ f a
instance (Monoid w) => Monad2 (Writer w) where
    mmv >>== f = 
        mmv >>= \(Writer (a, w)) -> 
        (\(Writer (b, w')) -> Writer (b, w <> w')) |$> f a
instance (Monoid w) => Monad3 (Writer w) where
    mmv >>>== f = 
        mmv >>== \(Writer (a, w)) -> 
        (\(Writer (b, w')) -> Writer (b, w <> w')) |$>> f a
instance (Monoid w) => Monad4 (Writer w) where
    mmv >>>>== f = 
        mmv >>>== \(Writer (a, w)) -> 
        (\(Writer (b, w')) -> Writer (b, w <> w')) |$>>> f a
instance (Monoid w) => Monad5 (Writer w) where
    mmv >>>>>== f = 
        mmv >>>>== \(Writer (a, w)) -> 
        (\(Writer (b, w')) -> Writer (b, w <> w')) |$>>>> f a

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

mapWriter :: ((a, w) -> (b, w')) -> Writer w a -> Writer w' b
mapWriter f m = Writer $ f (runWriter m)

----------------------------------------------------------------------
-- Level-1

newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

instance (Monad m) => Functor (WriterT w m) where
    fmap f v = WriterT $ (\(a, w) -> (f a, w)) |$> (runWriterT v)
instance (Monoid w, Monad m) => Applicative (WriterT w m) where
    pure a = WriterT $ (*:) (a, mempty)
    (<*>)  = ap
instance (Monoid w, Monad m) => Monad (WriterT w m) where  
    return = pure
    (WriterT v) >>= f = WriterT $
        v >>= \(a, w) ->
        runWriterT (f a) >>= \(a', w') ->
        (*:) (a', w <> w')
instance (Monoid w, Monad m) => MonadWriter w (WriterT w m) where
    writer   = WriterT . (*:)
    tell w   = writer $ ((), w)
    listen m = WriterT $
        runWriterT m >>= \(a, w) ->
        (*:) ((a, w), w) 
    pass m   = WriterT $
        runWriterT m >>= \((a, f), w) ->
        (*:) (a, f w)

instance (Monoid w) => MonadTrans (WriterT w) where
    trans m = WriterT $ 
        m >>= \a ->
        (*:) (a, mempty)
instance (Monoid w, MonadIO m, Monad m) => MonadIO (WriterT w m) where
    liftIO = trans . liftIO

execWriterT :: (Monad m) => WriterT w m a -> m w
execWriterT m =
    runWriterT m >>= \(_, w) ->
    (*:) w

mapWriterT :: (m (a, w) -> n (b, w')) -> WriterT w m a -> WriterT w' n b
mapWriterT f m = WriterT $ f (runWriterT m)

liftCatch :: Catch e m (a,w) -> Catch e (WriterT w m) a
liftCatch catchE m h =
    WriterT $ runWriterT m `catchE` \ e -> runWriterT (h e)

----------------------------------------------------------------------
-- Level-2

newtype WriterT2 w m1 m2 a = WriterT2 { runWriterT2 :: m1 (m2 (a, w)) }

instance (Monad m1, Monad2 m2) => Functor (WriterT2 w m1 m2) where
    fmap f v = WriterT2 $ (\(a, w) -> (f a, w)) |$>> (runWriterT2 v)
instance (Monoid w, Monad m1, Monad2 m2) => Applicative (WriterT2 w m1 m2) where
    pure a = WriterT2 $ (**:) (a, mempty)
    (<*>)  = ap
instance (Monoid w, Monad m1, Monad2 m2) => Monad (WriterT2 w m1 m2) where  
    return = pure
    (WriterT2 v) >>= f = WriterT2 $
        v >>== \(a, w) ->
        runWriterT2 (f a) >>== \(a', w') ->
        (**:) (a', w <> w')
instance (Monoid w, Monad m1, Monad2 m2) => MonadWriter w (WriterT2 w m1 m2) where
    writer   = WriterT2 . (**:)
    tell w   = writer $ ((), w)
    listen m = WriterT2 $
        runWriterT2 m >>== \(a, w) ->
        (**:) ((a, w), w) 
    pass m   = WriterT2 $
        runWriterT2 m >>== \((a, f), w) ->
        (**:) (a, f w)

instance (Monoid w) => MonadTrans2 (WriterT2 w) where
    trans2 m = WriterT2 $ 
        m >>== \a ->
        (**:) (a, mempty)
instance (Monoid w, MonadIO m1, Monad m1, Monad2 m2) => MonadIO (WriterT2 w m1 m2) where
    liftIO = trans2 . (-*) . liftIO

execWriterT2 :: (Monad m1, Monad2 m2) => WriterT2 w m1 m2 a -> m1 (m2 w)
execWriterT2 m =
    runWriterT2 m >>== \(_, w) ->
    (**:) w

mapWriterT2 :: (m1 (m2 (a, w)) -> n1 (n2 (b, w'))) -> WriterT2 w m1 m2 a -> WriterT2 w' n1 n2 b
mapWriterT2 f m = WriterT2 $ f (runWriterT2 m)

----------------------------------------------------------------------
-- Level-3

newtype WriterT3 w m1 m2 m3 a = WriterT3 { runWriterT3 :: m1 (m2 (m3 (a, w))) }

instance (Monad m1, Monad2 m2, Monad3 m3) => Functor (WriterT3 w m1 m2 m3) where
    fmap f v = WriterT3 $ (\(a, w) -> (f a, w)) |$>>> (runWriterT3 v)
instance (Monoid w, Monad m1, Monad2 m2, Monad3 m3) => Applicative (WriterT3 w m1 m2 m3) where
    pure a = WriterT3 $ (***:) (a, mempty)
    (<*>)  = ap
instance (Monoid w, Monad m1, Monad2 m2, Monad3 m3) => Monad (WriterT3 w m1 m2 m3) where  
    return = pure
    (WriterT3 v) >>= f = WriterT3 $
        v >>>== \(a, w) ->
        runWriterT3 (f a) >>>== \(a', w') ->
        (***:) (a', w <> w')
instance (Monoid w, Monad m1, Monad2 m2, Monad3 m3) => MonadWriter w (WriterT3 w m1 m2 m3) where
    writer   = WriterT3 . (***:)
    tell w   = writer $ ((), w)
    listen m = WriterT3 $
        runWriterT3 m >>>== \(a, w) ->
        (***:) ((a, w), w) 
    pass m   = WriterT3 $
        runWriterT3 m >>>== \((a, f), w) ->
        (***:) (a, f w)

instance (Monoid w) => MonadTrans3 (WriterT3 w) where
    trans3 m = WriterT3 $ 
        m >>>== \a ->
        (***:) (a, mempty)
instance (Monoid w, MonadIO m1, Monad m1, Monad2 m2, Monad3 m3) => MonadIO (WriterT3 w m1 m2 m3) where
    liftIO = trans3 . (-**) . liftIO

execWriterT3 :: (Monad m1, Monad2 m2, Monad3 m3) => WriterT3 w m1 m2 m3 a -> m1 (m2 (m3 w))
execWriterT3 m =
    runWriterT3 m >>>== \(_, w) ->
    (***:) w

mapWriterT3 :: (m1 (m2 (m3 (a, w))) -> n1 (n2 (n3 (b, w')))) -> WriterT3 w m1 m2 m3 a -> WriterT3 w' n1 n2 n3 b
mapWriterT3 f m = WriterT3 $ f (runWriterT3 m)


