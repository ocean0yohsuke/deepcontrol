{-|
Module      : DeepControl.Monad.Trans.Identity
Description : Enables dealing with deep monads in monad-transformer
Copyright   : (c) 2007 Magnus Therning,
              (c) 2015 KONISHI Yohsuke
License     : BSD-style (see the file LICENSE)
Maintainer  : ocean0yohsuke@gmail.com
Stability   : experimental
Portability : ---

This module enables you to deal with deep monads in any monad-transformer.
-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module DeepControl.Monad.Trans.Identity (
    -- * Level-1
    module Data.Functor.Identity,
    module Control.Monad.Trans.Identity,

    -- * Level-1
    (|*|),

    -- * Level-2
    IdentityT2(..),
    -- ** lift functions
    mapT2, liftCallCC2, liftCatch2,
    -- ** trans-cover  
    (|**|),
    (|-*|), (|*-|),
    -- ** trans-fold  
    transfold2, untransfold2,

    -- * Level-3
    IdentityT3(..),
    -- ** lift functions
    mapT3, liftCallCC3, liftCatch3,
    -- ** trans-cover  
    (|***|),
    (|--*|), (|-*-|), (|*--|),
    (|-**|), (|*-*|), (|**-|),
    -- ** trans-fold  
    transfold3, untransfold3,

    -- * Level-4
    IdentityT4(..),
    -- ** lift functions
    mapT4, liftCallCC4, liftCatch4,
    -- ** trans-cover
    (|****|),
    (|---*|), (|--*-|), (|-*--|), (|*---|),
    (|--**|), (|-*-*|), (|*--*|), (|*-*-|), (|-**-|), (|**--|), 
    (|-***|), (|*-**|), (|**-*|), (|***-|), 
    -- ** trans-fold  
    transfold4, untransfold4,

    -- * Level-5
    IdentityT5(..),
    -- ** lift functions
    mapT5, liftCallCC5, liftCatch5,
    -- ** trans-cover
    (|*****|),
    (|----*|), (|---*-|), (|--*--|), (|-*---|), (|*----|),
    (|---**|), (|--*-*|), (|-*--*|), (|*---*|), (|*--*-|), (|-*-*-|), (|--**-|), (|-**--|), (|*-*--|), (|**---|),
    (|--***|), (|-*-**|), (|*--**|), (|*-*-*|), (|-**-*|), (|**--*|), (|**-*-|), (|*-**-|), (|-***-|), (|***--|),
    (|-****|), (|*-***|), (|**-**|), (|***-*|), (|****-|),
    -- ** trans-fold  
    transfold5, untransfold5,

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

infixl 3  |*|
(|*|) :: (Monad m) => Identity a -> IdentityT m a
(|*|) = IdentityT . (*:) . runIdentity
    
----------------------------------------------------------------------
-- Level-2

newtype IdentityT2 f1 f2 a = IdentityT2 { runIdentityT2 :: f1 (f2 a) }
    deriving (Functor, Eq, Ord, Read, Show, Foldable, Traversable)

instance (Applicative m1, Applicative m2) => Applicative (IdentityT2 m1 m2) where
    pure x = IdentityT2 $ (**:) x
    (<*>) = liftIdentityT2 (|*>>)
instance (Commutative f1, Commutative f2) => Commutative (IdentityT2 f1 f2) where
    commute = (IdentityT2|$>) . float2 . runIdentityT2
instance (Monad m1, Monad m2, Commutative m2) => Monad (IdentityT2 m1 m2) where
    return = IdentityT2 . (**:)
    m >>= f = IdentityT2 $ runIdentityT2 m >>== (f >-> runIdentityT2)

instance (Alternative m1, Alternative m2) => Alternative (IdentityT2 m1 m2) where
    empty = IdentityT2 empty
    (<|>) = liftIdentityT2 (<|>)
instance (MonadPlus m1, Alternative m2, Monad m2, Commutative m2) => MonadPlus (IdentityT2 m1 m2) where
    mzero = IdentityT2 mzero
    mplus = liftIdentityT2 mplus

instance MonadTrans2 IdentityT2 where
    liftT2 = IdentityT2
instance (MonadIO m1, Monad m1, Monad m2, Commutative m2) => MonadIO (IdentityT2 m1 m2) where
    liftIO = liftT2 . (-*) . liftIO

liftIdentityT2 :: (m1 (m2 a) -> n1 (n2 b) -> p1 (p2 c)) -> IdentityT2 m1 m2 a -> IdentityT2 n1 n2 b -> IdentityT2 p1 p2 c
liftIdentityT2 f a b = IdentityT2 (f (runIdentityT2 a) (runIdentityT2 b))

mapT2 :: (m1 (m2 a) -> n1 (n2 b)) -> IdentityT2 m1 m2 a -> IdentityT2 n1 n2 b
mapT2 f = IdentityT2 . f . runIdentityT2

liftCallCC2 :: CallCC2 m1 m2 a b -> CallCC (IdentityT2 m1 m2) a b
liftCallCC2 callCC f = IdentityT2 $ callCC $ \c -> runIdentityT2 $ (c >-> IdentityT2) >- f

liftCatch2 :: Catch2 e m1 m2 a -> Catch e (IdentityT2 m1 m2) a
liftCatch2 catch m h = IdentityT2 $ (runIdentityT2 m) `catch` (h >-> runIdentityT2)

----------

transfold2 :: (Monad m1, Monad (t2 m1), MonadTrans_ m2 t2) => IdentityT2 m1 m2 a -> IdentityT (t2 m1) a
transfold2 (IdentityT2 x) = IdentityT $ trans x
untransfold2 :: (Monad m1, Monad (t2 m1), MonadTrans_ m2 t2) => IdentityT (t2 m1) a -> IdentityT2 m1 m2 a
untransfold2 (IdentityT x) = IdentityT2 $ untrans x

infixl 3  |**|
(|**|) :: (Monad m1, Monad m2) => Identity a -> IdentityT2 m1 m2 a
(|**|) = IdentityT2 . (**:) . runIdentity
infixl 3  |-*|, |*-|
(|-*|) :: (Monad m1, Monad m2) => IdentityT m1 a -> IdentityT2 m1 m2 a
(|-*|) = IdentityT2 . (-*) . runIdentityT
(|*-|) :: (Monad m1, Monad m2) => IdentityT m2 a -> IdentityT2 m1 m2 a
(|*-|) = IdentityT2 . (*:) . runIdentityT

----------------------------------------------------------------------
-- Level-3

newtype IdentityT3 f1 f2 f3 a = IdentityT3 { runIdentityT3 :: f1 (f2 (f3 a)) }
    deriving (Functor, Eq, Ord, Read, Show, Foldable, Traversable)

instance (Applicative m1, Applicative m2, Applicative m3) => Applicative (IdentityT3 m1 m2 m3) where
    pure x = IdentityT3 $ (***:) x
    (<*>) = lift3IdentityT3 (|*>>>)
instance (Commutative f1, Commutative f2, Commutative f3) => Commutative (IdentityT3 f1 f2 f3) where
    commute = (IdentityT3|$>) . float3 . runIdentityT3
instance (Monad m1, Monad m2, Commutative m2, Monad m3, Commutative m3) => Monad (IdentityT3 m1 m2 m3) where
    return = IdentityT3 . (***:)
    m >>= f = IdentityT3 $ runIdentityT3 m >>>== (f >-> runIdentityT3)

instance (Alternative m1, Alternative m2, Alternative m3) => Alternative (IdentityT3 m1 m2 m3) where
    empty = IdentityT3 empty
    (<|>) = lift3IdentityT3 (<|>)
instance (MonadPlus m1, Alternative m2, Monad m2, Commutative m2, Alternative m3, Monad m3, Commutative m3) => MonadPlus (IdentityT3 m1 m2 m3) where
    mzero = IdentityT3 mzero
    mplus = lift3IdentityT3 mplus

instance MonadTrans3 IdentityT3 where
    liftT3 = IdentityT3
instance (MonadIO m1, Monad m1, Monad m2, Commutative m2, Monad m3, Commutative m3) => MonadIO (IdentityT3 m1 m2 m3) where
    liftIO = liftT3 . (-**) . liftIO

lift3IdentityT3 ::
    (m1 (m2 (m3 a)) -> n1 (n2 (n3 b)) -> p1 (p2 (p3 c))) -> IdentityT3 m1 m2 m3 a -> IdentityT3 n1 n2 n3 b -> IdentityT3 p1 p2 p3 c
lift3IdentityT3 f a b = IdentityT3 (f (runIdentityT3 a) (runIdentityT3 b))

mapT3 :: (m1 (m2 (m3 a)) -> n1 (n2 (n3 b))) -> IdentityT3 m1 m2 m3 a -> IdentityT3 n1 n2 n3 b
mapT3 f = IdentityT3 . f . runIdentityT3

liftCallCC3 :: CallCC3 m1 m2 m3 a b -> CallCC (IdentityT3 m1 m2 m3) a b
liftCallCC3 callCC f = IdentityT3 $ callCC $ \c -> runIdentityT3 $ (c >-> IdentityT3) >- f

liftCatch3 :: Catch3 e m1 m2 m3 a -> Catch e (IdentityT3 m1 m2 m3) a
liftCatch3 catch m h = IdentityT3 $ (runIdentityT3 m) `catch` (h >-> runIdentityT3)

-------------

transfold3 :: (Monad m1, Monad (t2 m1), MonadTrans_ m2 t2, Monad (t3 (t2 m1)), MonadTrans_ m3 t3) => IdentityT3 m1 m2 m3 a -> IdentityT (t3 (t2 m1)) a
transfold3 (IdentityT3 x) = IdentityT $ trans . trans $  x
untransfold3 :: (Monad m1, Monad (t2 m1), MonadTrans_ m2 t2, Monad (t3 (t2 m1)), MonadTrans_ m3 t3) => IdentityT (t3 (t2 m1)) a -> IdentityT3 m1 m2 m3 a
untransfold3 (IdentityT x) = IdentityT3 $ untrans . untrans $ x

infixl 3  |***|
(|***|) :: (Monad m1, Monad m2, Monad m3) => Identity a -> IdentityT3 m1 m2 m3 a
(|***|) = IdentityT3 . (***:) . runIdentity

infixl 3  |--*|, |-*-|, |*--|
(|--*|) :: (Monad m1, Monad m2, Monad m3) => IdentityT2 m1 m2 a -> IdentityT3 m1 m2 m3 a
(|--*|) = IdentityT3 . (--*) . runIdentityT2
(|-*-|) :: (Monad m1, Monad m2, Monad m3) => IdentityT2 m1 m3 a -> IdentityT3 m1 m2 m3 a
(|-*-|) = IdentityT3 . (-*) . runIdentityT2
(|*--|) :: (Monad m1, Monad m2, Monad m3) => IdentityT2 m2 m3 a -> IdentityT3 m1 m2 m3 a
(|*--|) = IdentityT3 . (*:) . runIdentityT2

infixl 3  |-**|, |*-*|, |**-|
(|-**|) :: (Monad m1, Monad m2, Monad m3) => IdentityT m1 a -> IdentityT3 m1 m2 m3 a
(|-**|) = (|--*|) . (|-*|)
(|*-*|) :: (Monad m1, Monad m2, Monad m3) => IdentityT m2 a -> IdentityT3 m1 m2 m3 a
(|*-*|) = (|--*|) . (|*-|)
(|**-|) :: (Monad m1, Monad m2, Monad m3) => IdentityT m3 a -> IdentityT3 m1 m2 m3 a
(|**-|) = (|-*-|) . (|*-|)

----------------------------------------------------------------------
-- Level-4

newtype IdentityT4 f1 f2 f3 f4 a = IdentityT4 { runIdentityT4 :: f1 (f2 (f3 (f4 a))) }
    deriving (Functor, Eq, Ord, Read, Show, Foldable, Traversable)

instance (Applicative m1, Applicative m2, Applicative m3, Applicative m4) => Applicative (IdentityT4 m1 m2 m3 m4) where
    pure x = IdentityT4 $ (****:) x
    (<*>) = lift4IdentityT4 (|*>>>>)
instance (Commutative f1, Commutative f2, Commutative f3, Commutative f4) => Commutative (IdentityT4 f1 f2 f3 f4) where
    commute = (IdentityT4|$>) . float4 . runIdentityT4
instance (Monad m1, Monad m2, Commutative m2, Monad m3, Commutative m3, Monad m4, Commutative m4) => Monad (IdentityT4 m1 m2 m3 m4) where
    return = IdentityT4 . (****:)
    m >>= f = IdentityT4 $ runIdentityT4 m >>>>== (f >-> runIdentityT4)

instance (Alternative m1, Alternative m2, Alternative m3, Alternative m4) => Alternative (IdentityT4 m1 m2 m3 m4) where
    empty = IdentityT4 empty
    (<|>) = lift4IdentityT4 (<|>)
instance (MonadPlus m1, Alternative m2, Monad m2, Commutative m2, Alternative m3, Monad m3, Commutative m3, Alternative m4, Monad m4, Commutative m4) => MonadPlus (IdentityT4 m1 m2 m3 m4) where
    mzero = IdentityT4 mzero
    mplus = lift4IdentityT4 mplus

instance MonadTrans4 IdentityT4 where
    liftT4 = IdentityT4
instance (MonadIO m1, Monad m1, Monad m2, Commutative m2, Monad m3, Commutative m3, Monad m4, Commutative m4) => MonadIO (IdentityT4 m1 m2 m3 m4) where
    liftIO = liftT4 . (-***) . liftIO

lift4IdentityT4 :: (m1 (m2 (m3 (m4 a))) -> n1 (n2 (n3 (n4 b))) -> p1 (p2 (p3 (p4 c)))) -> IdentityT4 m1 m2 m3 m4 a -> IdentityT4 n1 n2 n3 n4 b -> IdentityT4 p1 p2 p3 p4 c
lift4IdentityT4 f a b = IdentityT4 (f (runIdentityT4 a) (runIdentityT4 b))

mapT4 :: (m1 (m2 (m3 (m4 a))) -> n1 (n2 (n3 (n4 b)))) -> IdentityT4 m1 m2 m3 m4 a -> IdentityT4 n1 n2 n3 n4 b
mapT4 f = IdentityT4 . f . runIdentityT4

liftCallCC4 :: CallCC4 m1 m2 m3 m4 a b -> CallCC (IdentityT4 m1 m2 m3 m4) a b
liftCallCC4 callCC f = IdentityT4 $ callCC $ \c -> runIdentityT4 $ (c >-> IdentityT4) >- f

liftCatch4 :: Catch4 e m1 m2 m3 m4 a -> Catch e (IdentityT4 m1 m2 m3 m4) a
liftCatch4 catch m h = IdentityT4 $ (runIdentityT4 m) `catch` (h >-> runIdentityT4)

-------------

transfold4 :: (Monad m1, Monad (t2 m1), MonadTrans_ m2 t2, Monad (t3 (t2 m1)), MonadTrans_ m3 t3, Monad (t4 (t3 (t2 m1))), MonadTrans_ m4 t4) => IdentityT4 m1 m2 m3 m4 a -> IdentityT (t4 (t3 (t2 m1))) a
transfold4 (IdentityT4 x) = IdentityT $ trans . trans . trans $  x
untransfold4 :: (Monad m1, Monad (t2 m1), MonadTrans_ m2 t2, Monad (t3 (t2 m1)), MonadTrans_ m3 t3, Monad (t4 (t3 (t2 m1))), MonadTrans_ m4 t4) => IdentityT (t4 (t3 (t2 m1))) a -> IdentityT4 m1 m2 m3 m4 a
untransfold4 (IdentityT x) = IdentityT4 $ untrans . untrans . untrans $ x

infixl 3  |****|
(|****|) :: (Monad m1, Monad m2, Monad m3, Monad m4) => Identity a -> IdentityT4 m1 m2 m3 m4 a
(|****|) = IdentityT4 . (****:) . runIdentity

infixl 3  |---*|, |--*-|, |-*--|, |*---|
(|---*|) :: (Monad m1, Monad m2, Monad m3, Monad m4) => IdentityT3 m1 m2 m3 a -> IdentityT4 m1 m2 m3 m4 a
(|---*|) = IdentityT4 . (---*) . runIdentityT3
(|--*-|) :: (Monad m1, Monad m2, Monad m3, Monad m4) => IdentityT3 m1 m2 m4 a -> IdentityT4 m1 m2 m3 m4 a
(|--*-|) = IdentityT4 . (--*) . runIdentityT3
(|-*--|) :: (Monad m1, Monad m2, Monad m3, Monad m4) => IdentityT3 m1 m3 m4 a -> IdentityT4 m1 m2 m3 m4 a
(|-*--|) = IdentityT4 . (-*) . runIdentityT3
(|*---|) :: (Monad m1, Monad m2, Monad m3, Monad m4) => IdentityT3 m2 m3 m4 a -> IdentityT4 m1 m2 m3 m4 a
(|*---|) = IdentityT4 . (*:) . runIdentityT3

infixl 3  |--**|, |-*-*|, |-**-|, |*-*-|, |**--|, |*--*|
(|--**|) :: (Monad m1, Monad m2, Monad m3, Monad m4) => IdentityT2 m1 m2 a -> IdentityT4 m1 m2 m3 m4 a
(|--**|) = (|---*|) . (|--*|)
(|-*-*|) :: (Monad m1, Monad m2, Monad m3, Monad m4) => IdentityT2 m1 m3 a -> IdentityT4 m1 m2 m3 m4 a
(|-*-*|) = (|---*|) . (|-*-|)
(|-**-|) :: (Monad m1, Monad m2, Monad m3, Monad m4) => IdentityT2 m1 m4 a -> IdentityT4 m1 m2 m3 m4 a
(|-**-|) = (|--*-|) . (|-*-|)
(|*-*-|) :: (Monad m1, Monad m2, Monad m3, Monad m4) => IdentityT2 m2 m4 a -> IdentityT4 m1 m2 m3 m4 a
(|*-*-|) = (|--*-|) . (|*--|)
(|**--|) :: (Monad m1, Monad m2, Monad m3, Monad m4) => IdentityT2 m3 m4 a -> IdentityT4 m1 m2 m3 m4 a
(|**--|) = (|-*--|) . (|*--|)
(|*--*|) :: (Monad m1, Monad m2, Monad m3, Monad m4) => IdentityT2 m2 m3 a -> IdentityT4 m1 m2 m3 m4 a
(|*--*|) = (|---*|) . (|*--|)

infixl 3  |-***|, |*-**|, |**-*|, |***-| 
(|-***|) :: (Monad m1, Monad m2, Monad m3, Monad m4) => IdentityT m1 a -> IdentityT4 m1 m2 m3 m4 a
(|-***|) = (|---*|) . (|-**|)
(|*-**|) :: (Monad m1, Monad m2, Monad m3, Monad m4) => IdentityT m2 a -> IdentityT4 m1 m2 m3 m4 a
(|*-**|) = (|---*|) . (|*-*|)
(|**-*|) :: (Monad m1, Monad m2, Monad m3, Monad m4) => IdentityT m3 a -> IdentityT4 m1 m2 m3 m4 a
(|**-*|) = (|---*|) . (|**-|)
(|***-|) :: (Monad m1, Monad m2, Monad m3, Monad m4) => IdentityT m4 a -> IdentityT4 m1 m2 m3 m4 a
(|***-|) = (|--*-|) . (|**-|)

----------------------------------------------------------------------
-- Level-5

newtype IdentityT5 f1 f2 f3 f4 f5 a = IdentityT5 { runIdentityT5 :: f1 (f2 (f3 (f4 (f5 a)))) }
    deriving (Functor, Eq, Ord, Read, Show, Foldable, Traversable)

instance (Commutative f1, Commutative f2, Commutative f3, Commutative f4, Commutative f5) => Commutative (IdentityT5 f1 f2 f3 f4 f5) where
    commute = (IdentityT5|$>) . float5 . runIdentityT5

instance (Applicative m1, Applicative m2, Applicative m3, Applicative m4, Applicative m5) => Applicative (IdentityT5 m1 m2 m3 m4 m5) where
    pure x = IdentityT5 $ (*****:) x
    (<*>) = lift5IdentityT5 (|*>>>>>)
instance (Monad m1, Monad m2, Commutative m2, Monad m3, Commutative m3, Monad m4, Commutative m4, Monad m5, Commutative m5) => Monad (IdentityT5 m1 m2 m3 m4 m5) where
    return = IdentityT5 . (*****:)
    m >>= f = IdentityT5 $ runIdentityT5 m >>>>>== (f >-> runIdentityT5)

instance (Alternative m1, Alternative m2, Alternative m3, Alternative m4, Alternative m5) => Alternative (IdentityT5 m1 m2 m3 m4 m5) where
    empty = IdentityT5 empty
    (<|>) = lift5IdentityT5 (<|>)
instance (MonadPlus m1, Alternative m2, Monad m2, Commutative m2, Alternative m3, Monad m3, Commutative m3, Alternative m4, Monad m4, Commutative m4, Alternative m5, Monad m5, Commutative m5) => MonadPlus (IdentityT5 m1 m2 m3 m4 m5) where
    mzero = IdentityT5 mzero
    mplus = lift5IdentityT5 mplus

instance MonadTrans5 IdentityT5 where
    liftT5 = IdentityT5
instance (MonadIO m1, Monad m1, Monad m2, Commutative m2, Monad m3, Commutative m3, Monad m4, Commutative m4, Monad m5, Commutative m5) => MonadIO (IdentityT5 m1 m2 m3 m4 m5) where
    liftIO = liftT5 . (-****) . liftIO

lift5IdentityT5 ::
    (m1 (m2 (m3 (m4 (m5 a)))) -> n1 (n2 (n3 (n4 (n5 b)))) -> p1 (p2 (p3 (p4 (p5 c))))) -> IdentityT5 m1 m2 m3 m4 m5 a -> IdentityT5 n1 n2 n3 n4 n5 b -> IdentityT5 p1 p2 p3 p4 p5 c
lift5IdentityT5 f a b = IdentityT5 (f (runIdentityT5 a) (runIdentityT5 b))

mapT5 :: (m1 (m2 (m3 (m4 (m5 a)))) -> n1 (n2 (n3 (n4 (n5 b))))) -> IdentityT5 m1 m2 m3 m4 m5 a -> IdentityT5 n1 n2 n3 n4 n5 b
mapT5 f = IdentityT5 . f . runIdentityT5

liftCallCC5 :: CallCC5 m1 m2 m3 m4 m5 a b -> CallCC (IdentityT5 m1 m2 m3 m4 m5) a b
liftCallCC5 callCC f = IdentityT5 $ callCC $ \c -> runIdentityT5 $ (c >-> IdentityT5) >- f

liftCatch5 :: Catch5 e m1 m2 m3 m4 m5 a -> Catch e (IdentityT5 m1 m2 m3 m4 m5) a
liftCatch5 catch m h = IdentityT5 $ (runIdentityT5 m) `catch` (h >-> runIdentityT5)

-------------

transfold5 :: (Monad m1, Monad (t2 m1), MonadTrans_ m2 t2, Monad (t3 (t2 m1)), MonadTrans_ m3 t3, Monad (t4 (t3 (t2 m1))), MonadTrans_ m4 t4, Monad (t5 (t4 (t3 (t2 m1)))), MonadTrans_ m5 t5) => IdentityT5 m1 m2 m3 m4 m5 a -> IdentityT (t5 (t4 (t3 (t2 m1)))) a
transfold5 (IdentityT5 x) = IdentityT $ trans . trans . trans . trans $  x
untransfold5 :: (Monad m1, Monad (t2 m1), MonadTrans_ m2 t2, Monad (t3 (t2 m1)), MonadTrans_ m3 t3, Monad (t4 (t3 (t2 m1))), MonadTrans_ m4 t4, Monad (t5 (t4 (t3 (t2 m1)))), MonadTrans_ m5 t5) => IdentityT (t5 (t4 (t3 (t2 m1)))) a -> IdentityT5 m1 m2 m3 m4 m5 a
untransfold5 (IdentityT x) = IdentityT5 $ untrans . untrans . untrans . untrans $ x

infixl 3  |*****|
(|*****|) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => Identity a -> IdentityT5 m1 m2 m3 m4 m5 a
(|*****|) = IdentityT5 . (*****:) . runIdentity

infixl 3  |----*|, |---*-|, |--*--|, |-*---|, |*----|
(|----*|) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT4 m1 m2 m3 m4 a -> IdentityT5 m1 m2 m3 m4 m5 a
(|----*|) = IdentityT5 . (----*) . runIdentityT4
(|---*-|) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT4 m1 m2 m3 m5 a -> IdentityT5 m1 m2 m3 m4 m5 a
(|---*-|) = IdentityT5 . (---*) . runIdentityT4
(|--*--|) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT4 m1 m2 m4 m5 a -> IdentityT5 m1 m2 m3 m4 m5 a
(|--*--|) = IdentityT5 . (--*) . runIdentityT4
(|-*---|) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT4 m1 m3 m4 m5 a -> IdentityT5 m1 m2 m3 m4 m5 a
(|-*---|) = IdentityT5 . (-*) . runIdentityT4
(|*----|) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT4 m2 m3 m4 m5 a -> IdentityT5 m1 m2 m3 m4 m5 a
(|*----|) = IdentityT5 . (*:) . runIdentityT4

infixl 3  |---**|, |--*-*|, |-*--*|, |*---*|, |*--*-|, |-*-*-|, |--**-|, |-**--|, |*-*--|, |**---|
(|---**|) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT3 m1 m2 m3 a -> IdentityT5 m1 m2 m3 m4 m5 a
(|---**|) = (|----*|) . (|---*|)
(|--*-*|) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT3 m1 m2 m4 a -> IdentityT5 m1 m2 m3 m4 m5 a
(|--*-*|) = (|----*|) . (|--*-|)
(|-*--*|) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT3 m1 m3 m4 a -> IdentityT5 m1 m2 m3 m4 m5 a
(|-*--*|) = (|----*|) . (|-*--|)
(|*---*|) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT3 m2 m3 m4 a -> IdentityT5 m1 m2 m3 m4 m5 a
(|*---*|) = (|----*|) . (|*---|)
(|*--*-|) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT3 m2 m3 m5 a -> IdentityT5 m1 m2 m3 m4 m5 a
(|*--*-|) = (|---*-|) . (|*---|)
(|-*-*-|) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT3 m1 m3 m5 a -> IdentityT5 m1 m2 m3 m4 m5 a
(|-*-*-|) = (|---*-|) . (|-*--|)
(|--**-|) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT3 m1 m2 m5 a -> IdentityT5 m1 m2 m3 m4 m5 a
(|--**-|) = (|---*-|) . (|--*-|)
(|-**--|) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT3 m1 m4 m5 a -> IdentityT5 m1 m2 m3 m4 m5 a
(|-**--|) = (|--*--|) . (|-*--|)
(|*-*--|) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT3 m2 m4 m5 a -> IdentityT5 m1 m2 m3 m4 m5 a
(|*-*--|) = (|*----|) . (|-*--|)
(|**---|) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT3 m3 m4 m5 a -> IdentityT5 m1 m2 m3 m4 m5 a
(|**---|) = (|-*---|) . (|*---|)
infixl 3  |--***|, |-*-**|, |*--**|, |*-*-*|, |-**-*|, |**--*|, |**-*-|, |*-**-|, |-***-|, |***--|
(|--***|) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT2 m1 m2 a -> IdentityT5 m1 m2 m3 m4 m5 a
(|--***|) = (|----*|) . (|--**|)
(|-*-**|) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT2 m1 m3 a -> IdentityT5 m1 m2 m3 m4 m5 a
(|-*-**|) = (|----*|) . (|-*-*|)
(|*--**|) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT2 m2 m3 a -> IdentityT5 m1 m2 m3 m4 m5 a
(|*--**|) = (|----*|) . (|*--*|)
(|*-*-*|) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT2 m2 m4 a -> IdentityT5 m1 m2 m3 m4 m5 a
(|*-*-*|) = (|----*|) . (|*-*-|)
(|-**-*|) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT2 m1 m4 a -> IdentityT5 m1 m2 m3 m4 m5 a
(|-**-*|) = (|----*|) . (|-**-|)
(|**--*|) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT2 m3 m4 a -> IdentityT5 m1 m2 m3 m4 m5 a
(|**--*|) = (|----*|) . (|**--|)
(|**-*-|) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT2 m3 m5 a -> IdentityT5 m1 m2 m3 m4 m5 a
(|**-*-|) = (|---*-|) . (|**--|)
(|*-**-|) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT2 m2 m5 a -> IdentityT5 m1 m2 m3 m4 m5 a
(|*-**-|) = (|*----|) . (|-**-|)
(|-***-|) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT2 m1 m5 a -> IdentityT5 m1 m2 m3 m4 m5 a
(|-***-|) = (|---*-|) . (|-**-|)
(|***--|) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT2 m4 m5 a -> IdentityT5 m1 m2 m3 m4 m5 a
(|***--|) = (|--*--|) . (|**--|)
infixl 3  |-****|, |*-***|, |**-**|, |***-*|, |****-|
(|-****|) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT m1 a -> IdentityT5 m1 m2 m3 m4 m5 a
(|-****|) = (|----*|) . (|-***|)
(|*-***|) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT m2 a -> IdentityT5 m1 m2 m3 m4 m5 a
(|*-***|) = (|----*|) . (|*-**|)
(|**-**|) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT m3 a -> IdentityT5 m1 m2 m3 m4 m5 a
(|**-**|) = (|----*|) . (|**-*|)
(|***-*|) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT m4 a -> IdentityT5 m1 m2 m3 m4 m5 a
(|***-*|) = (|----*|) . (|***-|)
(|****-|) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT m5 a -> IdentityT5 m1 m2 m3 m4 m5 a
(|****-|) = (|---*-|) . (|***-|)

