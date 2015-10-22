{-|
Module      : DeepControl.Monad.Trans.Identity
Description : ---
Copyright   : (c) 2007 Magnus Therning,
              (C) 2015 KONISHI Yohsuke
License     : BSD-style (see the file LICENSE)
Maintainer  : ocean0yohsuke@gmail.com
Stability   : experimental
Portability : ---


-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module DeepControl.Monad.Trans.Identity (
    module Data.Functor.Identity,
    module Control.Monad.Trans.Identity,

    -- * Level-2
    IdentityT2(..),
    -- ** lift functions
    mapIdentityT2, liftCallCC2, liftCatch2,

    -- * Level-3
    IdentityT3(..),
    -- ** lift functions
    mapIdentityT3, liftCallCC3, liftCatch3,

{-
    -- * Level-4
    IdentityT4(..),
    -- ** lift functions
    mapIdentityT4, liftCallCC4, liftCatch4,

    -- * Level-5
    IdentityT5(..),
    -- ** lift functions
    mapIdentityT5, liftCallCC5, liftCatch5,
-}

    ) where 

import DeepControl.Applicative
import DeepControl.Commutative
import DeepControl.Monad
import DeepControl.Monad.Trans
import DeepControl.Monad.Signatures

import Data.Functor.Identity
import Control.Monad.Trans.Identity

----------------------------------------------------------------------
-- Level-1

instance (Commutative f) => Commutative (IdentityT f) where
    commute = (IdentityT|$>) . commute . runIdentityT

instance MonadTransDown IdentityT where
    type TransDown IdentityT = Identity

instance MonadTransCover IdentityT where
    (|*|) = IdentityT . (*:) . runIdentity

----------------------------------------------------------------------
-- Level-2

newtype IdentityT2 f1 f2 a = IdentityT2 { runIdentityT2 :: f1 (f2 a) }
    deriving (Functor, Eq, Ord, Read, Show, Foldable, Traversable)

instance (Commutative f1, Commutative f2) => Commutative (IdentityT2 f1 f2) where
    commute = (IdentityT2|$>) . float2 . runIdentityT2

instance (Applicative m1, Applicative m2) => Applicative (IdentityT2 m1 m2) where
    pure x = IdentityT2 $ (**:) x
    (<*>) = lift2IdentityT2 (|*>>)
instance (Monad m1, Monad2 m2) => Monad (IdentityT2 m1 m2) where
    return = IdentityT2 . (**:)
    m >>= f = IdentityT2 $ runIdentityT2 m >>== (f >-> runIdentityT2)

instance (Alternative m1, Alternative m2) => Alternative (IdentityT2 m1 m2) where
    empty = IdentityT2 empty
    (<|>) = lift2IdentityT2 (<|>)
instance (MonadPlus m1, Alternative m2, Monad2 m2) => MonadPlus (IdentityT2 m1 m2) where
    mzero = IdentityT2 mzero
    mplus = lift2IdentityT2 mplus

instance MonadTrans2 IdentityT2 where
    lift2 = IdentityT2
instance (MonadIO m1, Monad m1, Monad2 m2) => MonadIO (IdentityT2 m1 m2) where
    liftIO = lift2 . (-*) . liftIO

lift2IdentityT2 ::
    (m1 (m2 a) -> n1 (n2 b) -> p1 (p2 c)) -> IdentityT2 m1 m2 a -> IdentityT2 n1 n2 b -> IdentityT2 p1 p2 c
lift2IdentityT2 f a b = IdentityT2 (f (runIdentityT2 a) (runIdentityT2 b))

mapIdentityT2 :: (m1 (m2 a) -> n1 (n2 b)) -> IdentityT2 m1 m2 a -> IdentityT2 n1 n2 b
mapIdentityT2 f = IdentityT2 . f . runIdentityT2

liftCallCC2 :: CallCC2 m1 m2 a b -> CallCC (IdentityT2 m1 m2) a b
liftCallCC2 callCC f = IdentityT2 $ callCC $ \c -> runIdentityT2 $ (c >-> IdentityT2) >- f

liftCatch2 :: Catch2 e m1 m2 a -> Catch e (IdentityT2 m1 m2) a
liftCatch2 catch m h = IdentityT2 $ (runIdentityT2 m) `catch` (h >-> runIdentityT2)

instance MonadTrans2Down IdentityT2 where
    type Trans2Down IdentityT2 = IdentityT

instance MonadTransFold2 IdentityT2 where
    transfold2 (IdentityT2 x) = IdentityT $ trans x
    untransfold2 (IdentityT x) = IdentityT2 $ untrans x

instance MonadTransCover2 IdentityT2 where
    (|-*|) = IdentityT2 . (-*) . runIdentityT
    (|*-|) = IdentityT2 . (*-) . runIdentityT

----------------------------------------------------------------------
-- Level-3

newtype IdentityT3 f1 f2 f3 a = IdentityT3 { runIdentityT3 :: f1 (f2 (f3 a)) }
    deriving (Functor, Eq, Ord, Read, Show, Foldable, Traversable)

instance (Commutative f1, Commutative f2, Commutative f3) => Commutative (IdentityT3 f1 f2 f3) where
    commute = (IdentityT3|$>) . float3 . runIdentityT3

instance (Applicative m1, Applicative m2, Applicative m3) => Applicative (IdentityT3 m1 m2 m3) where
    pure x = IdentityT3 $ (***:) x
    (<*>) = lift3IdentityT3 (|*>>>)
instance (Monad m1, Monad2 m2, Monad3 m3) => Monad (IdentityT3 m1 m2 m3) where
    return = IdentityT3 . (***:)
    m >>= f = IdentityT3 $ runIdentityT3 m >>>== (f >-> runIdentityT3)

instance (Alternative m1, Alternative m2, Alternative m3) => Alternative (IdentityT3 m1 m2 m3) where
    empty = IdentityT3 empty
    (<|>) = lift3IdentityT3 (<|>)
instance (MonadPlus m1, Alternative m2, Monad2 m2, Alternative m3, Monad3 m3) => MonadPlus (IdentityT3 m1 m2 m3) where
    mzero = IdentityT3 mzero
    mplus = lift3IdentityT3 mplus

instance MonadTrans3 IdentityT3 where
    lift3 = IdentityT3
instance (MonadIO m1, Monad m1, Monad2 m2, Monad3 m3) => MonadIO (IdentityT3 m1 m2 m3) where
    liftIO = lift3 . (-**) . liftIO

lift3IdentityT3 ::
    (m1 (m2 (m3 a)) -> n1 (n2 (n3 b)) -> p1 (p2 (p3 c))) -> IdentityT3 m1 m2 m3 a -> IdentityT3 n1 n2 n3 b -> IdentityT3 p1 p2 p3 c
lift3IdentityT3 f a b = IdentityT3 (f (runIdentityT3 a) (runIdentityT3 b))

mapIdentityT3 :: (m1 (m2 (m3 a)) -> n1 (n2 (n3 b))) -> IdentityT3 m1 m2 m3 a -> IdentityT3 n1 n2 n3 b
mapIdentityT3 f = IdentityT3 . f . runIdentityT3

liftCallCC3 :: CallCC3 m1 m2 m3 a b -> CallCC (IdentityT3 m1 m2 m3) a b
liftCallCC3 callCC f = IdentityT3 $ callCC $ \c -> runIdentityT3 $ (c >-> IdentityT3) >- f

liftCatch3 :: Catch3 e m1 m2 m3 a -> Catch e (IdentityT3 m1 m2 m3) a
liftCatch3 catch m h = IdentityT3 $ (runIdentityT3 m) `catch` (h >-> runIdentityT3)

instance MonadTrans3Down IdentityT3 where
    type Trans3Down IdentityT3 = IdentityT2

instance MonadTransFold3 IdentityT3 where
    transfold3 (IdentityT3 x) = IdentityT $ trans2 x
    untransfold3 (IdentityT x) = IdentityT3 $ untrans2 x

instance MonadTransCover3 IdentityT3 where
    (|--*|) = IdentityT3 . (--*) . runIdentityT2
    (|-*-|) = IdentityT3 . (-*-) . runIdentityT2
    (|*--|) = IdentityT3 . (*--) . runIdentityT2

