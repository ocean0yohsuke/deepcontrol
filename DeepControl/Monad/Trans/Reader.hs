{-|
Module      : DeepControl.Monad.Trans.Reader
Description : Extension for mtl's Contrl.Monad.Reader.
Copyright   : (c) Andy Gill 2001,
              (c) Oregon Graduate Institute of Science and Technology 2001,
              (c) Jeff Newbern 2003-2007,
              (c) Andriy Palamarchuk 2007,
              (C) 2015 KONISHI Yohsuke,
License     : BSD-style (see the file LICENSE)
Maintainer  : ocean0yohsuke@gmail.com
Stability   : experimental
Portability : ---

This module extended Reader Monad of mtl(monad-transformer-library).
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
module DeepControl.Monad.Trans.Reader (
    module Control.Monad.Reader,

    -- * Level-2
    ReaderT2(..), mapReaderT2, 
    -- ** lift function
    liftCatch2,

    -- * Level-3
    ReaderT3(..), mapReaderT3, 
    -- ** lift function
    liftCatch3,
    
    ) where 

import DeepControl.Applicative
import DeepControl.Monad
import DeepControl.Monad.Signatures
import DeepControl.Monad.Trans

import Control.Monad.Reader 

----------------------------------------------------------------------
-- Level-1

instance MonadTransDown (ReaderT r) where
    type TransDown (ReaderT r) = Reader r

instance MonadTransCover (ReaderT s) where
    (|*|) = ReaderT . ((*:)|$>) . runReader

----------------------------------------------------------------------
-- Level-2

newtype ReaderT2 r m1 m2 a = ReaderT2 { runReaderT2 :: r -> m1 (m2 a) }
    deriving (Functor)
{-
instance (Functor m1, Functor m2) => Functor (ReaderT2 r m1 m2) where
    fmap f m = ReaderT2 $ \r ->
        f |$>> runReaderT2 m r
-}
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
    lift2 m = ReaderT2 $ \r -> 
        m >>== \a ->
        (**:) a
instance (MonadIO m1, Monad m1, Monad2 m2) => MonadIO (ReaderT2 r m1 m2) where
    liftIO = lift2 . (-*) . liftIO

instance MonadTrans2Down (ReaderT2 r) where
    type Trans2Down (ReaderT2 r) = ReaderT r

instance MonadTransFold2 (ReaderT2 r) where
    transfold2 (ReaderT2 x) = ReaderT $ x <$| trans
    untransfold2 (ReaderT x) = ReaderT2 $ x <$| untrans

instance MonadTransCover2 (ReaderT2 r) where
    (|-*|) = ReaderT2 . ((-*)|$>) . runReaderT
    (|*-|) = ReaderT2 . ((*-)|$>) . runReaderT

mapReaderT2 :: (m1 (m2 a) -> n1 (n2 b)) -> ReaderT2 r m1 m2 a -> ReaderT2 r n1 n2 b
mapReaderT2 f m = ReaderT2 $ f . runReaderT2 m

liftCatch2 :: Catch2 e m1 m2 a -> Catch e (ReaderT2 r m1 m2) a
liftCatch2 catch m h = ReaderT2 $ \r -> (runReaderT2 m r) `catch` (\e -> runReaderT2 (h e) r)

instance (Alternative m1, Alternative m2, Monad m1, Monad2 m2) => Alternative (ReaderT2 r m1 m2) where
    empty   = ReaderT2 (const empty)
    m <|> n = ReaderT2 $ \r -> runReaderT2 m r <$|(<|>)|*> runReaderT2 n r
instance (MonadPlus m1, MonadPlus m2, Monad m1, Monad2 m2) => MonadPlus (ReaderT2 r m1 m2) where
    mzero       = lift2 mzero
    m `mplus` n = ReaderT2 $ \r -> runReaderT2 m r <$|mplus|*> runReaderT2 n r

----------------------------------------------------------------------
-- Level-3

newtype ReaderT3 r m1 m2 m3 a = ReaderT3 { runReaderT3 :: r -> m1 (m2 (m3 a)) }
    deriving (Functor)
{-
instance (Functor m1, Functor m2, Functor m3) => Functor (ReaderT3 r m1 m2 m3) where
    fmap f m = ReaderT3 $ \r ->
        f |$>>> runReaderT3 m r
-}
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
    lift3 m = ReaderT3 $ \r -> 
        m >>>== \a ->
        (***:) a
instance (MonadIO m1, Monad m1, Monad2 m2, Monad3 m3) => MonadIO (ReaderT3 r m1 m2 m3) where
    liftIO = lift3 . (-**) . liftIO

instance MonadTrans3Down (ReaderT3 r) where
    type Trans3Down (ReaderT3 r) = ReaderT2 r

instance MonadTransFold3 (ReaderT3 r) where
    transfold3 (ReaderT3 x) = ReaderT $ x <$| trans2
    untransfold3 (ReaderT x) = ReaderT3 $ x <$| untrans2

instance MonadTransCover3 (ReaderT3 r) where
    (|--*|) = ReaderT3 . ((--*)|$>) . runReaderT2
    (|-*-|) = ReaderT3 . ((-*-)|$>) . runReaderT2
    (|*--|) = ReaderT3 . ((*--)|$>) . runReaderT2

mapReaderT3 :: (m1 (m2 (m3 a)) -> n1 (n2 (n3 b))) -> ReaderT3 r m1 m2 m3 a -> ReaderT3 r n1 n2 n3 b
mapReaderT3 f m = ReaderT3 $ f . runReaderT3 m

liftCatch3 :: Catch3 e m1 m2 m3 a -> Catch e (ReaderT3 r m1 m2 m3) a
liftCatch3 catch m h = ReaderT3 $ \r -> (runReaderT3 m r) `catch` (\e -> runReaderT3 (h e) r)

instance (MonadPlus m1, MonadPlus m2, MonadPlus m3, Monad m1, Monad2 m2, Monad3 m3) => MonadPlus (ReaderT3 r m1 m2 m3) where
    mzero       = lift3 mzero
    m `mplus` n = ReaderT3 $ \r -> runReaderT3 m r <<$|mplus|*>> runReaderT3 n r
instance (Alternative m1, Alternative m2, Alternative m3, Monad m1, Monad2 m2, Monad3 m3) => Alternative (ReaderT3 r m1 m2 m3) where
    empty   = ReaderT3 (const empty)
    m <|> n = ReaderT3 $ \r -> runReaderT3 m r <<$|(<|>)|*>> runReaderT3 n r

