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
    -- * Level-1
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

    -- * Level-4
    IdentityT4(..),
    -- ** lift functions
    mapIdentityT4, liftCallCC4, liftCatch4,

    -- * Level-5
    IdentityT5(..),
    -- ** lift functions
    mapIdentityT5, liftCallCC5, liftCatch5,

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
    liftT2 = IdentityT2
instance (MonadIO m1, Monad m1, Monad2 m2) => MonadIO (IdentityT2 m1 m2) where
    liftIO = liftT2 . (-*) . liftIO

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
    (|*-|) = IdentityT2 . (*:) . runIdentityT

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
    liftT3 = IdentityT3
instance (MonadIO m1, Monad m1, Monad2 m2, Monad3 m3) => MonadIO (IdentityT3 m1 m2 m3) where
    liftIO = liftT3 . (-**) . liftIO

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
    (|-*-|) = IdentityT3 . (-*) . runIdentityT2
    (|*--|) = IdentityT3 . (*:) . runIdentityT2

----------------------------------------------------------------------
-- Level-4

newtype IdentityT4 f1 f2 f3 f4 a = IdentityT4 { runIdentityT4 :: f1 (f2 (f3 (f4 a))) }
    deriving (Functor, Eq, Ord, Read, Show, Foldable, Traversable)

instance (Commutative f1, Commutative f2, Commutative f3, Commutative f4) => Commutative (IdentityT4 f1 f2 f3 f4) where
    commute = (IdentityT4|$>) . float4 . runIdentityT4

instance (Applicative m1, Applicative m2, Applicative m3, Applicative m4) => Applicative (IdentityT4 m1 m2 m3 m4) where
    pure x = IdentityT4 $ (****:) x
    (<*>) = lift4IdentityT4 (|*>>>>)
instance (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => Monad (IdentityT4 m1 m2 m3 m4) where
    return = IdentityT4 . (****:)
    m >>= f = IdentityT4 $ runIdentityT4 m >>>>== (f >-> runIdentityT4)

instance (Alternative m1, Alternative m2, Alternative m3, Alternative m4) => Alternative (IdentityT4 m1 m2 m3 m4) where
    empty = IdentityT4 empty
    (<|>) = lift4IdentityT4 (<|>)
instance (MonadPlus m1, Alternative m2, Monad2 m2, Alternative m3, Monad3 m3, Alternative m4, Monad4 m4) => MonadPlus (IdentityT4 m1 m2 m3 m4) where
    mzero = IdentityT4 mzero
    mplus = lift4IdentityT4 mplus

instance MonadTrans4 IdentityT4 where
    liftT4 = IdentityT4
instance (MonadIO m1, Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => MonadIO (IdentityT4 m1 m2 m3 m4) where
    liftIO = liftT4 . (-***) . liftIO

lift4IdentityT4 ::
    (m1 (m2 (m3 (m4 a))) -> n1 (n2 (n3 (n4 b))) -> p1 (p2 (p3 (p4 c)))) -> IdentityT4 m1 m2 m3 m4 a -> IdentityT4 n1 n2 n3 n4 b -> IdentityT4 p1 p2 p3 p4 c
lift4IdentityT4 f a b = IdentityT4 (f (runIdentityT4 a) (runIdentityT4 b))

mapIdentityT4 :: (m1 (m2 (m3 (m4 a))) -> n1 (n2 (n3 (n4 b)))) -> IdentityT4 m1 m2 m3 m4 a -> IdentityT4 n1 n2 n3 n4 b
mapIdentityT4 f = IdentityT4 . f . runIdentityT4

liftCallCC4 :: CallCC4 m1 m2 m3 m4 a b -> CallCC (IdentityT4 m1 m2 m3 m4) a b
liftCallCC4 callCC f = IdentityT4 $ callCC $ \c -> runIdentityT4 $ (c >-> IdentityT4) >- f

liftCatch4 :: Catch4 e m1 m2 m3 m4 a -> Catch e (IdentityT4 m1 m2 m3 m4) a
liftCatch4 catch m h = IdentityT4 $ (runIdentityT4 m) `catch` (h >-> runIdentityT4)

instance MonadTrans4Down IdentityT4 where
    type Trans4Down IdentityT4 = IdentityT3

instance MonadTransFold4 IdentityT4 where
    transfold4 (IdentityT4 x) = IdentityT $ trans3 x
    untransfold4 (IdentityT x) = IdentityT4 $ untrans3 x

instance MonadTransCover4 IdentityT4 where
    (|---*|) = IdentityT4 . (---*) . runIdentityT3
    (|--*-|) = IdentityT4 . (--*) . runIdentityT3
    (|-*--|) = IdentityT4 . (-*) . runIdentityT3
    (|*---|) = IdentityT4 . (*:) . runIdentityT3

----------------------------------------------------------------------
-- Level-5

newtype IdentityT5 f1 f2 f3 f4 f5 a = IdentityT5 { runIdentityT5 :: f1 (f2 (f3 (f4 (f5 a)))) }
    deriving (Functor, Eq, Ord, Read, Show, Foldable, Traversable)

instance (Commutative f1, Commutative f2, Commutative f3, Commutative f4, Commutative f5) => Commutative (IdentityT5 f1 f2 f3 f4 f5) where
    commute = (IdentityT5|$>) . float5 . runIdentityT5

instance (Applicative m1, Applicative m2, Applicative m3, Applicative m4, Applicative m5) => Applicative (IdentityT5 m1 m2 m3 m4 m5) where
    pure x = IdentityT5 $ (*****:) x
    (<*>) = lift5IdentityT5 (|*>>>>>)
instance (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => Monad (IdentityT5 m1 m2 m3 m4 m5) where
    return = IdentityT5 . (*****:)
    m >>= f = IdentityT5 $ runIdentityT5 m >>>>>== (f >-> runIdentityT5)

instance (Alternative m1, Alternative m2, Alternative m3, Alternative m4, Alternative m5) => Alternative (IdentityT5 m1 m2 m3 m4 m5) where
    empty = IdentityT5 empty
    (<|>) = lift5IdentityT5 (<|>)
instance (MonadPlus m1, Alternative m2, Monad2 m2, Alternative m3, Monad3 m3, Alternative m4, Monad4 m4, Alternative m5, Monad5 m5) => MonadPlus (IdentityT5 m1 m2 m3 m4 m5) where
    mzero = IdentityT5 mzero
    mplus = lift5IdentityT5 mplus

instance MonadTrans5 IdentityT5 where
    liftT5 = IdentityT5
instance (MonadIO m1, Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => MonadIO (IdentityT5 m1 m2 m3 m4 m5) where
    liftIO = liftT5 . (-****) . liftIO

lift5IdentityT5 ::
    (m1 (m2 (m3 (m4 (m5 a)))) -> n1 (n2 (n3 (n4 (n5 b)))) -> p1 (p2 (p3 (p4 (p5 c))))) -> IdentityT5 m1 m2 m3 m4 m5 a -> IdentityT5 n1 n2 n3 n4 n5 b -> IdentityT5 p1 p2 p3 p4 p5 c
lift5IdentityT5 f a b = IdentityT5 (f (runIdentityT5 a) (runIdentityT5 b))

mapIdentityT5 :: (m1 (m2 (m3 (m4 (m5 a)))) -> n1 (n2 (n3 (n4 (n5 b))))) -> IdentityT5 m1 m2 m3 m4 m5 a -> IdentityT5 n1 n2 n3 n4 n5 b
mapIdentityT5 f = IdentityT5 . f . runIdentityT5

liftCallCC5 :: CallCC5 m1 m2 m3 m4 m5 a b -> CallCC (IdentityT5 m1 m2 m3 m4 m5) a b
liftCallCC5 callCC f = IdentityT5 $ callCC $ \c -> runIdentityT5 $ (c >-> IdentityT5) >- f

liftCatch5 :: Catch5 e m1 m2 m3 m4 m5 a -> Catch e (IdentityT5 m1 m2 m3 m4 m5) a
liftCatch5 catch m h = IdentityT5 $ (runIdentityT5 m) `catch` (h >-> runIdentityT5)

instance MonadTrans5Down IdentityT5 where
    type Trans5Down IdentityT5 = IdentityT4

instance MonadTransFold5 IdentityT5 where
    transfold5 (IdentityT5 x) = IdentityT $ trans4 x
    untransfold5 (IdentityT x) = IdentityT5 $ untrans4 x

instance MonadTransCover5 IdentityT5 where
    (|----*|) = IdentityT5 . (----*) . runIdentityT4
    (|---*-|) = IdentityT5 . (---*) . runIdentityT4
    (|--*--|) = IdentityT5 . (--*) . runIdentityT4
    (|-*---|) = IdentityT5 . (-*) . runIdentityT4
    (|*----|) = IdentityT5 . (*:) . runIdentityT4


