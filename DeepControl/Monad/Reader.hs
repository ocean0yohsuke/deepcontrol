{-|
Module      : DeepControl.Monad.Reader
Description : 
Copyright   : (c) Andy Gill 2001,
              (c) Oregon Graduate Institute of Science and Technology 2001,
              (c) Jeff Newbern 2003-2007,
              (c) Andriy Palamarchuk 2007,
              (C) 2015 KONISHI Yohsuke,
License     : BSD-style (see the file LICENSE)
Maintainer  : ocean0yohsuke@gmail.com
Stability   : experimental
Portability : ---

This module is just a concise mimic for Reader Monad in mtl(monad-transformer-library).
The qualifier "concise" means that this module doesn't make no attempt to transform functions of any kind of Monad automatically.
So when making some new data type of ReaderT, you have to manually define involved Monad instances, 
for example `DeepControl.Monad.MonadError`, 
by making use of the transformation functions such as `trans`, `trans2`, etc.
Admittedly it is tedious though, you can deeply understand monad-transformation mechanism instead.
-}
{-# LANGUAGE MultiParamTypeClasses, 
             FlexibleInstances #-}
module DeepControl.Monad.Reader (
    MonadReader(..),
    asks,

    -- * Level-0
    Reader(..),
    -- * Level-1
    ReaderT(..), mapReaderT, liftCatch,
    -- * Level-2
    ReaderT2(..), mapReaderT2,
    -- * Level-3
    ReaderT3(..), mapReaderT3,
    
    ) where 

import DeepControl.Applicative
import DeepControl.Monad
import DeepControl.MonadTrans

import Control.Monad.Reader (MonadReader(..))
import Control.Monad.Signatures

asks :: MonadReader r m => (r -> a) -> m a
asks = reader

----------------------------------------------------------------------
-- Level-0

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
    fmap f v = Reader $ \r -> 
        f $ runReader v r 
instance Applicative (Reader r) where
    pure a = Reader $ \_ -> a
    (<*>)  = ap
instance Monad (Reader r) where
    return = pure
    mv >>= f = mv >- \(Reader v) -> Reader $ \r ->
        v r >- \a -> 
        runReader (f a) r

{-
instance Monad2 (Reader r) where
    -- TODO: Reader を Commutative にできていないため、これは無理
    mmv >>== f = mmv >>= \(Reader v) -> commute $ Reader $ \r ->
        v r >- \a ->
        runReader |$> f a |* r
-}  

instance MonadReader r (Reader r) where
    ask       = Reader id
    local f m = Reader $ runReader m . f

----------------------------------------------------------------------
-- Level-1

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
    fmap f m = ReaderT $ \r ->
        f |$> runReaderT m r
instance (Monad m) => Applicative (ReaderT s m) where
    pure a = ReaderT $ \_ -> (*:) a
    (<*>)  = ap
instance (Monad m) => Monad (ReaderT r m) where
    return = pure
    (ReaderT v) >>= f = ReaderT $ \r ->
        v r >>= \a -> 
        runReaderT (f a) r
instance (Monad m) => MonadReader r (ReaderT r m) where
    ask       = ReaderT $ (*:)
    local f m = ReaderT $ runReaderT m . f

instance MonadTrans (ReaderT r) where
    trans m = ReaderT $ \r -> 
        m >>= \a ->
        (*:) a
instance (MonadIO m, Monad m) => MonadIO (ReaderT r m) where
    liftIO = trans . liftIO

mapReaderT :: (m a -> n b) -> ReaderT r m a -> ReaderT r n b
mapReaderT f m = ReaderT $ f . runReaderT m

liftCatch :: Catch e m a -> Catch e (ReaderT r m) a
liftCatch catch m h =
    ReaderT $ \ r -> runReaderT m r `catch` \ e -> runReaderT (h e) r

----------------------------------------------------------------------
-- Level-2

newtype ReaderT2 r m1 m2 a = ReaderT2 { runReaderT2 :: r -> m1 (m2 a) }

instance (Functor m1, Functor m2) => Functor (ReaderT2 r m1 m2) where
    fmap f m = ReaderT2 $ \r ->
        f |$>> runReaderT2 m r
instance (Monad m1, Monad2 m2) => Applicative (ReaderT2 s m1 m2) where
    pure a = ReaderT2 $ \_ -> (**:) a
    (<*>)  = ap
instance (Monad m1, Monad2 m2) => Monad (ReaderT2 r m1 m2) where
    return = pure
    (ReaderT2 v) >>= f = ReaderT2 $ \r ->
        v r >>== \a -> 
        runReaderT2 (f a) r
instance (Monad m1, Monad2 m2) => MonadReader r (ReaderT2 r m1 m2) where
    ask       = ReaderT2 $ (**:)
    local f m = ReaderT2 $ runReaderT2 m . f

instance MonadTrans2 (ReaderT2 r) where
    trans2 m = ReaderT2 $ \r -> 
        m >>== \a ->
        (**:) a
instance (MonadIO m1, Monad m1, Monad2 m2) => MonadIO (ReaderT2 r m1 m2) where
    liftIO = trans2 . (-*) . liftIO

mapReaderT2 :: (m1 (m2 a) -> n1 (n2 b)) -> ReaderT2 r m1 m2 a -> ReaderT2 r n1 n2 b
mapReaderT2 f m = ReaderT2 $ f . runReaderT2 m

----------------------------------------------------------------------
-- Level-3

newtype ReaderT3 r m1 m2 m3 a = ReaderT3 { runReaderT3 :: r -> m1 (m2 (m3 a)) }

instance (Functor m1, Functor m2, Functor m3) => Functor (ReaderT3 r m1 m2 m3) where
    fmap f m = ReaderT3 $ \r ->
        f |$>>> runReaderT3 m r
instance (Monad m1, Monad2 m2, Monad3 m3) => Applicative (ReaderT3 s m1 m2 m3) where
    pure a = ReaderT3 $ \_ -> (***:) a
    (<*>)  = ap
instance (Monad m1, Monad2 m2, Monad3 m3) => Monad (ReaderT3 r m1 m2 m3) where
    return = pure
    (ReaderT3 v) >>= f = ReaderT3 $ \r ->
        v r >>>== \a -> 
        runReaderT3 (f a) r
instance (Monad m1, Monad2 m2, Monad3 m3) => MonadReader r (ReaderT3 r m1 m2 m3) where
    ask       = ReaderT3 $ (***:)
    local f m = ReaderT3 $ runReaderT3 m . f

instance MonadTrans3 (ReaderT3 r) where
    trans3 m = ReaderT3 $ \r -> 
        m >>>== \a ->
        (***:) a
instance (MonadIO m1, Monad m1, Monad2 m2, Monad3 m3) => MonadIO (ReaderT3 r m1 m2 m3) where
    liftIO = trans3 . (-**) . liftIO

mapReaderT3 :: (m1 (m2 (m3 a)) -> n1 (n2 (n3 b))) -> ReaderT3 r m1 m2 m3 a -> ReaderT3 r n1 n2 n3 b
mapReaderT3 f m = ReaderT3 $ f . runReaderT3 m

