{-|
Module      : DeepControl.Monad.Trans.State
Description : Extension for mtl's Contrl.Monad.Writer.
Copyright   : (c) Andy Gill 2001,
              (c) Oregon Graduate Institute of Science and Technology, 2001,
              (C) 2015 KONISHI Yohsuke,
License     : BSD-style (see the file LICENSE)
Maintainer  : ocean0yohsuke@gmail.com
Stability   : experimental
Portability : ---

This module extended Writer Monad of mtl(monad-transformer-library).
-}
{-# LANGUAGE FlexibleInstances #-}
module DeepControl.Monad.Trans.Writer (
    module Control.Monad.Writer,

    -- * Level-2
    listen2, pass2,
    -- * Level-3
    listen3, pass3,
    -- * Level-4
    --listen4, pass4,
    -- * Level-5
    --listen5, pass5,

{-
    -- * Level-2
    WriterT2(..), execWriterT2, mapWriterT2, liftCatch2,
    -- * Level-3
    WriterT3(..), execWriterT3, mapWriterT3, liftCatch3,
-}
    ) where 

import DeepControl.Applicative
import DeepControl.Commutative
import DeepControl.Monad
import DeepControl.Monad.Signatures
import DeepControl.Monad.Trans

import Control.Monad.Writer
import Control.Monad.Identity
import Data.Monoid

----------------------------------------------------------------------
-- Level-1

instance (Monoid w) => Commutative (Writer w) where
    commute x = 
        let (a, b) = runWriter x
        in  (WriterT . Identity) |$> (a <$|(,)|* b)

----------------------------------------------------------------------
-- Level-2

instance (Monoid w) => Monad2 (Writer w) where
    mv >>== f = 
        mv >>= \x -> runWriterT x >- \(Identity (a, w)) ->
        f a <$| (\x -> runWriterT x >- \(Identity (b, w')) ->
                       WriterT $ Identity (b, w <> w'))

listen2 :: (MonadWriter w m2, Applicative m1) => m1 (m2 a) -> m1 (m2 (a, w))
listen2 m = listen |$> m
pass2 :: (MonadWriter w m2, Applicative m1) => m1 (m2 (a, w -> w)) -> m1 (m2 a)
pass2 m = pass |$> m

----------------------------------------------------------------------
-- Level-3

instance (Monoid w) => Monad3 (Writer w) where
    mv >>>== f = 
        mv >>== \x -> runWriterT x >- \(Identity (a, w)) ->
        f a <<$| (\x -> runWriterT x >- \(Identity (b, w')) ->
                        WriterT $ Identity (b, w <> w'))

listen3 :: (MonadWriter w m3, Applicative m1, Applicative m2) => m1 (m2 (m3 a)) -> m1 (m2 (m3 (a, w)))
listen3 m = listen2 |$> m
pass3 :: (MonadWriter w m3, Applicative m1, Applicative m2) => m1 (m2 (m3 (a, w -> w))) -> m1 (m2 (m3 a))
pass3 m = pass2 |$> m

----------------------------------------------------------------------
-- Level-4

instance (Monoid w) => Monad4 (Writer w) where
    mv >>>>== f = 
        mv >>>== \x -> runWriterT x >- \(Identity (a, w)) ->
        f a <<<$| (\x -> runWriterT x >- \(Identity (b, w')) ->
                         WriterT $ Identity (b, w <> w'))

----------------------------------------------------------------------
-- Level-5

instance (Monoid w) => Monad5 (Writer w) where
    mv >>>>>== f = 
        mv >>>>== \x -> runWriterT x >- \(Identity (a, w)) ->
        f a <<<<$| (\x -> runWriterT x >- \(Identity (b, w')) ->
                          WriterT $ Identity (b, w <> w'))



{-
----------------------------------------------------------------------
-- Level-1


instance (Monoid w) => MonadTrans_ (WriterT w) where
    untrans x = (WriterT . Identity) |$> runWriterT x
    trans x   = WriterT ((runIdentity . runWriterT) |$> x)
instance (Monoid w) => MonadTransDown (WriterT w) where
    type TransDown (WriterT w) = Writer w
instance (Monoid w) => MonadTransCover (WriterT w) where
    (|*|) = WriterT . (*:) . runWriter

----------------------------------------------------------------------
-- Level-2


newtype WriterT2 w m1 m2 a = WriterT2 { runWriterT2 :: m1 (m2 (a, w)) }
    deriving (Functor)
{-
instance (Monad m1, Monad2 m2) => Functor (WriterT2 w m1 m2) where
    fmap f v = WriterT2 $ (\(a, w) -> (f a, w)) |$>> runWriterT2 v
-}
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
    lift2 m = WriterT2 $ 
        m >>== \a ->
        (**:) (a, mempty)
instance (Monoid w, MonadIO m1, Monad m1, Monad2 m2) => MonadIO (WriterT2 w m1 m2) where
    liftIO = lift2 . (-*) . liftIO

instance (Monoid w) => MonadTrans2Down (WriterT2 w) where
    type Trans2Down (WriterT2 w) = WriterT w
instance (Monoid w) => MonadTransFold2 (WriterT2 w) where
    transfold2 (WriterT2 x) = WriterT $ trans x
    untransfold2 (WriterT x) = WriterT2 $ untrans x
instance (Monoid w) => MonadTransCover2 (WriterT2 w) where
    (|-*|) = WriterT2 . (-*) . runWriterT
    (|*-|) = WriterT2 . (*-) . runWriterT

execWriterT2 :: (Monad m1, Monad2 m2) => WriterT2 w m1 m2 a -> m1 (m2 w)
execWriterT2 m =
    runWriterT2 m >>== \(_, w) ->
    (**:) w

mapWriterT2 :: (m1 (m2 (a, w)) -> n1 (n2 (b, w'))) -> WriterT2 w m1 m2 a -> WriterT2 w' n1 n2 b
mapWriterT2 f m = WriterT2 $ f (runWriterT2 m)

liftCatch2 :: Catch2 e m1 m2 (a,w) -> Catch e (WriterT2 w m1 m2) a
liftCatch2 catch m h = WriterT2 $ runWriterT2 m `catch` \e -> runWriterT2 (h e)

----------------------------------------------------------------------
-- Level-3

newtype WriterT3 w m1 m2 m3 a = WriterT3 { runWriterT3 :: m1 (m2 (m3 (a, w))) }
    deriving (Functor)
{-
instance (Monad m1, Monad2 m2, Monad3 m3) => Functor (WriterT3 w m1 m2 m3) where
    fmap f v = WriterT3 $ (\(a, w) -> (f a, w)) |$>>> runWriterT3 v
-}
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
    lift3 m = WriterT3 $ 
        m >>>== \a ->
        (***:) (a, mempty)
instance (Monoid w, MonadIO m1, Monad m1, Monad2 m2, Monad3 m3) => MonadIO (WriterT3 w m1 m2 m3) where
    liftIO = lift3 . (-**) . liftIO

instance (Monoid w) => MonadTrans3Down (WriterT3 w) where
    type Trans3Down (WriterT3 w) = WriterT2 w

instance (Monoid w) => MonadTransFold3 (WriterT3 w) where
    transfold3 (WriterT3 x) = WriterT $ trans2 x 
    untransfold3 (WriterT x) = WriterT3 $ untrans2 x 

instance (Monoid w) => MonadTransCover3 (WriterT3 w) where
    (|--*|) = WriterT3 . (--*) . runWriterT2
    (|-*-|) = WriterT3 . (-*-) . runWriterT2
    (|*--|) = WriterT3 . (*--) . runWriterT2

execWriterT3 :: (Monad m1, Monad2 m2, Monad3 m3) => WriterT3 w m1 m2 m3 a -> m1 (m2 (m3 w))
execWriterT3 m =
    runWriterT3 m >>>== \(_, w) ->
    (***:) w

mapWriterT3 :: (m1 (m2 (m3 (a, w))) -> n1 (n2 (n3 (b, w')))) -> WriterT3 w m1 m2 m3 a -> WriterT3 w' n1 n2 n3 b
mapWriterT3 f m = WriterT3 $ f (runWriterT3 m)

liftCatch3 :: Catch3 e m1 m2 m3 (a,w) -> Catch e (WriterT3 w m1 m2 m3) a
liftCatch3 catch m h = WriterT3 $ runWriterT3 m `catch` \e -> runWriterT3 (h e)
-}
