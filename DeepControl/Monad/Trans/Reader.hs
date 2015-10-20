{-|
Module      : DeepControl.Monad.Trans.Reader
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

This module extended Reader Monad in mtl(monad-transformer-library).
-}
{-# LANGUAGE MultiParamTypeClasses, 
             FlexibleInstances #-}
module DeepControl.Monad.Trans.Reader (
    module Control.Monad.Reader,

    -- * Level-2
    ReaderT2(..), mapReaderT2,
    -- * Level-3
    ReaderT3(..), mapReaderT3,
    
    ) where 

import DeepControl.Applicative
import DeepControl.Monad
import DeepControl.Monad.Trans

import Control.Monad.Reader 
import Control.Monad.Signatures

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
