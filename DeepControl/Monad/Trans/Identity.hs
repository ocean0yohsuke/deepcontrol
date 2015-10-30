{-|
Module      : DeepControl.Monad.Trans.Identity
Description : Deepened the usual Control.Monad.Trans.Identity module.
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
    -- ** identity-cover  
    (*:),

    -- * Level-2
    IdentityT2(..),
    -- ** identity-cover  
    (**:),
    (-*:), (*-:),
    -- ** identity-roll  
    transrollI2, untransrollI2,
    -- ** lift
    mapIdentityT2, liftCallCC2, liftCatch2,

    -- * Level-3
    IdentityT3(..),
    -- ** identity-cover  
    (***:),
    (--*:), (-*-:), (*--:),
    (-**:), (*-*:), (**-:),
    -- ** identity-roll  
    transrollI3, untransrollI3,
    -- ** lift 
    mapIdentityT3, liftCallCC3, liftCatch3,

    -- * Level-4
    IdentityT4(..),
    -- ** identity-cover
    (****:),
    (---*:), (--*-:), (-*--:), (*---:),
    (--**:), (-*-*:), (*--*:), (*-*-:), (-**-:), (**--:), 
    (-***:), (*-**:), (**-*:), (***-:), 
    -- ** identity-roll  
    transrollI4, untransrollI4,
    -- ** lift 
    mapIdentityT4, liftCallCC4, liftCatch4,

    -- * Level-5
    IdentityT5(..),
    -- ** identity-cover
    (*****:),
    (----*:), (---*-:), (--*--:), (-*---:), (*----:),
    (---**:), (--*-*:), (-*--*:), (*---*:), (*--*-:), (-*-*-:), (--**-:), (-**--:), (*-*--:), (**---:),
    (--***:), (-*-**:), (*--**:), (*-*-*:), (-**-*:), (**--*:), (**-*-:), (*-**-:), (-***-:), (***--:),
    (-****:), (*-***:), (**-**:), (***-*:), (****-:),
    -- ** identity-roll  
    transrollI5, untransrollI5,
    -- ** lift 
    mapIdentityT5, liftCallCC5, liftCatch5,

    -- * Example: identity-cover
    -- $Example

    ) where 

import DeepControl.Applicative
import DeepControl.Traversable
import DeepControl.Monad
import DeepControl.Monad.Trans
import DeepControl.Monad.Signatures

import Data.Functor.Identity
import Control.Monad.Trans.Identity

-- $setup
-- >>> import Control.Monad.Trans.Maybe
-- >>> import Control.Monad.List
-- >>> import Control.Monad.Except
-- >>> import Control.Monad.Writer

----------------------------------------------------------------------
-- Level-1

infixl 3  *:
-- | The level-1 identity-cover function, analogous to @'(.*)'@
--
-- >>> (*:) (Identity 1) :: IdentityT [] Int
-- IdentityT [1]
--
-- >>> (*:) (Identity 1) :: IdentityT Maybe Int
-- IdentityT (Just 1)
--
(*:) :: (Monad m) => Identity a -> IdentityT m a
(*:) = IdentityT . (.*) . runIdentity

----------------------------------------------------------------------
-- Level-2

newtype IdentityT2 f1 f2 a = IdentityT2 { runIdentityT2 :: f1 (f2 a) }
    deriving (Functor, Eq, Ord, Read, Show, Foldable, Traversable)

instance (Applicative m1, Applicative m2) => Applicative (IdentityT2 m1 m2) where
    pure x = IdentityT2 $ (.**) x
    (<*>) = lift2IdentityT2 (|*>>)
instance (Monad m1, Monad m2, Traversable m2) => Monad (IdentityT2 m1 m2) where
    return = IdentityT2 . (.**)
    m >>= f = IdentityT2 $ runIdentityT2 m >>== (f >-> runIdentityT2)

instance (Alternative m1, Alternative m2) => Alternative (IdentityT2 m1 m2) where
    empty = IdentityT2 empty
    (<|>) = lift2IdentityT2 (<|>)
instance (MonadPlus m1, Alternative m2, Monad m2, Traversable m2) => MonadPlus (IdentityT2 m1 m2) where
    mzero = IdentityT2 mzero
    mplus = lift2IdentityT2 mplus

instance (MonadIO m1, Monad m1, Monad m2, Traversable m2) => MonadIO (IdentityT2 m1 m2) where
    liftIO = IdentityT2 . (-*) . liftIO

transrollI2 :: (Monad m1, MonadTrans_ m2 t2) => IdentityT2 m1 m2 a -> IdentityT (t2 m1) a
transrollI2 = IdentityT . transroll2 . runIdentityT2
untransrollI2 :: (Monad m1, MonadTrans_ m2 t2) => IdentityT (t2 m1) a -> IdentityT2 m1 m2 a
untransrollI2 = IdentityT2 . untransroll2 . runIdentityT

lift2IdentityT2 :: (m1 (m2 a) -> n1 (n2 b) -> p1 (p2 c)) -> IdentityT2 m1 m2 a -> IdentityT2 n1 n2 b -> IdentityT2 p1 p2 c
lift2IdentityT2 f a b = IdentityT2 (f (runIdentityT2 a) (runIdentityT2 b))

mapIdentityT2 :: (m1 (m2 a) -> n1 (n2 b)) -> IdentityT2 m1 m2 a -> IdentityT2 n1 n2 b
mapIdentityT2 f = IdentityT2 . f . runIdentityT2

liftCallCC2 :: CallCC2 m1 m2 a b -> CallCC (IdentityT2 m1 m2) a b
liftCallCC2 callCC f = IdentityT2 $ callCC $ \c -> runIdentityT2 $ (c >-> IdentityT2) >- f

liftCatch2 :: Catch2 e m1 m2 a -> Catch e (IdentityT2 m1 m2) a
liftCatch2 catch m h = IdentityT2 $ (runIdentityT2 m) `catch` (h >-> runIdentityT2)

infixl 3  **:
-- | The level-2 identity-cover function, analogous to @'(**:)'@
--
-- >>> (**:) (Identity 1) :: IdentityT2 [] Maybe Int
-- IdentityT2 {runIdentityT2 = [Just 1]}
--
-- >>> (**:) (Identity 1) :: IdentityT2 (Except ()) (Writer String) Int
-- IdentityT2 {runIdentityT2 = ExceptT (Identity (Right (WriterT (Identity (1,"")))))}
--
(**:) :: (Monad m1, Monad m2) => Identity a -> IdentityT2 m1 m2 a
(**:) = IdentityT2 . (.**) . runIdentity
infixl 3  -*:, *-:
-- | The level-2 identity-cover function, analogous to @'(-*)'@
--
-- >>> (-*:) (IdentityT [1]) :: IdentityT2 [] Maybe Int
-- IdentityT2 {runIdentityT2 = [Just 1]}
--
-- >>> (-*:) (IdentityT (ExceptT (Identity (Right 1)))) :: IdentityT2 (Except ()) (Writer String) Int
-- IdentityT2 {runIdentityT2 = ExceptT (Identity (Right (WriterT (Identity (1,"")))))}
--
(-*:) :: (Monad m1, Monad m2) => IdentityT m1 a -> IdentityT2 m1 m2 a
(-*:) = IdentityT2 . (-*) . runIdentityT
-- | The level-2 identity-cover function, analogous to @'(.*)'@
--
-- >>> (*-:) (IdentityT (Just 1)) :: IdentityT2 [] Maybe Int
-- IdentityT2 {runIdentityT2 = [Just 1]}
--
-- >>> (*-:) (IdentityT (WriterT (Identity (1,"")))) :: IdentityT2 (Except ()) (Writer String) Int
-- IdentityT2 {runIdentityT2 = ExceptT (Identity (Right (WriterT (Identity (1,"")))))}
--
(*-:) :: (Monad m1, Monad m2) => IdentityT m2 a -> IdentityT2 m1 m2 a
(*-:) = IdentityT2 . (.*) . runIdentityT

----------------------------------------------------------------------
-- Level-3

newtype IdentityT3 f1 f2 f3 a = IdentityT3 { runIdentityT3 :: f1 (f2 (f3 a)) }
    deriving (Functor, Eq, Ord, Read, Show, Foldable, Traversable)

instance (Applicative m1, Applicative m2, Applicative m3) => Applicative (IdentityT3 m1 m2 m3) where
    pure x = IdentityT3 $ (.***) x
    (<*>) = lift2IdentityT3 (|*>>>)
instance (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3) => Monad (IdentityT3 m1 m2 m3) where
    return = IdentityT3 . (.***)
    m >>= f = IdentityT3 $ runIdentityT3 m >>>= (f >-> runIdentityT3)

instance (Alternative m1, Alternative m2, Alternative m3) => Alternative (IdentityT3 m1 m2 m3) where
    empty = IdentityT3 empty
    (<|>) = lift2IdentityT3 (<|>)
instance (MonadPlus m1, Alternative m2, Monad m2, Traversable m2, Alternative m3, Monad m3, Traversable m3) => MonadPlus (IdentityT3 m1 m2 m3) where
    mzero = IdentityT3 mzero
    mplus = lift2IdentityT3 mplus

instance (MonadIO m1, Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3) => MonadIO (IdentityT3 m1 m2 m3) where
    liftIO = IdentityT3 . (-**) . liftIO

transrollI3 :: (Monad m1, Monad (t2 m1), MonadTrans_ m2 t2, MonadTrans_ m3 t3) => IdentityT3 m1 m2 m3 a -> IdentityT (t3 (t2 m1)) a
transrollI3 = IdentityT . transroll3 . runIdentityT3
untransrollI3 :: (Monad m1, Monad (t2 m1), MonadTrans_ m2 t2, MonadTrans_ m3 t3) => IdentityT (t3 (t2 m1)) a -> IdentityT3 m1 m2 m3 a
untransrollI3 = IdentityT3 . untransroll3 . runIdentityT

lift2IdentityT3 :: (m1 (m2 (m3 a)) -> n1 (n2 (n3 b)) -> p1 (p2 (p3 c))) -> IdentityT3 m1 m2 m3 a -> IdentityT3 n1 n2 n3 b -> IdentityT3 p1 p2 p3 c
lift2IdentityT3 f a b = IdentityT3 (f (runIdentityT3 a) (runIdentityT3 b))

mapIdentityT3 :: (m1 (m2 (m3 a)) -> n1 (n2 (n3 b))) -> IdentityT3 m1 m2 m3 a -> IdentityT3 n1 n2 n3 b
mapIdentityT3 f = IdentityT3 . f . runIdentityT3

liftCallCC3 :: CallCC3 m1 m2 m3 a b -> CallCC (IdentityT3 m1 m2 m3) a b
liftCallCC3 callCC f = IdentityT3 $ callCC $ \c -> runIdentityT3 $ (c >-> IdentityT3) >- f

liftCatch3 :: Catch3 e m1 m2 m3 a -> Catch e (IdentityT3 m1 m2 m3) a
liftCatch3 catch m h = IdentityT3 $ (runIdentityT3 m) `catch` (h >-> runIdentityT3)

infixl 3  ***:
(***:) :: (Monad m1, Monad m2, Monad m3) => Identity a -> IdentityT3 m1 m2 m3 a
(***:) = IdentityT3 . (.***) . runIdentity

infixl 3  --*:, -*-:, *--:
(--*:) :: (Monad m1, Monad m2, Monad m3) => IdentityT2 m1 m2 a -> IdentityT3 m1 m2 m3 a
(--*:) = IdentityT3 . (--*) . runIdentityT2
(-*-:) :: (Monad m1, Monad m2, Monad m3) => IdentityT2 m1 m3 a -> IdentityT3 m1 m2 m3 a
(-*-:) = IdentityT3 . (-*) . runIdentityT2
(*--:) :: (Monad m1, Monad m2, Monad m3) => IdentityT2 m2 m3 a -> IdentityT3 m1 m2 m3 a
(*--:) = IdentityT3 . (.*) . runIdentityT2

infixl 3  -**:, *-*:, **-:
(-**:) :: (Monad m1, Monad m2, Monad m3) => IdentityT m1 a -> IdentityT3 m1 m2 m3 a
(-**:) = (--*:) . (-*:)
(*-*:) :: (Monad m1, Monad m2, Monad m3) => IdentityT m2 a -> IdentityT3 m1 m2 m3 a
(*-*:) = (--*:) . (*-:)
(**-:) :: (Monad m1, Monad m2, Monad m3) => IdentityT m3 a -> IdentityT3 m1 m2 m3 a
(**-:) = (-*-:) . (*-:)

----------------------------------------------------------------------
-- Level-4

newtype IdentityT4 f1 f2 f3 f4 a = IdentityT4 { runIdentityT4 :: f1 (f2 (f3 (f4 a))) }
    deriving (Functor, Eq, Ord, Read, Show, Foldable, Traversable)

instance (Applicative m1, Applicative m2, Applicative m3, Applicative m4) => Applicative (IdentityT4 m1 m2 m3 m4) where
    pure x = IdentityT4 $ (.****) x
    (<*>) = lift2IdentityT4 (|*>>>>)
instance (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4) => Monad (IdentityT4 m1 m2 m3 m4) where
    return = IdentityT4 . (.****)
    m >>= f = IdentityT4 $ runIdentityT4 m >>>>= (f >-> runIdentityT4)

instance (Alternative m1, Alternative m2, Alternative m3, Alternative m4) => Alternative (IdentityT4 m1 m2 m3 m4) where
    empty = IdentityT4 empty
    (<|>) = lift2IdentityT4 (<|>)
instance (MonadPlus m1, Alternative m2, Monad m2, Traversable m2, Alternative m3, Monad m3, Traversable m3, Alternative m4, Monad m4, Traversable m4) => MonadPlus (IdentityT4 m1 m2 m3 m4) where
    mzero = IdentityT4 mzero
    mplus = lift2IdentityT4 mplus

instance (MonadIO m1, Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4) => MonadIO (IdentityT4 m1 m2 m3 m4) where
    liftIO = IdentityT4 . (-***) . liftIO

transrollI4 :: (Monad m1, Monad (t2 m1), Monad (t3 (t2 m1)), MonadTrans_ m2 t2, MonadTrans_ m3 t3, MonadTrans_ m4 t4) => IdentityT4 m1 m2 m3 m4 a -> IdentityT (t4 (t3 (t2 m1))) a
transrollI4 = IdentityT . transroll4 . runIdentityT4
untransrollI4 :: (Monad m1, Monad (t2 m1), Monad (t3 (t2 m1)), MonadTrans_ m2 t2, MonadTrans_ m3 t3, MonadTrans_ m4 t4) => IdentityT (t4 (t3 (t2 m1))) a -> IdentityT4 m1 m2 m3 m4 a
untransrollI4 = IdentityT4 . untransroll4 . runIdentityT

lift2IdentityT4 :: (m1 (m2 (m3 (m4 a))) -> n1 (n2 (n3 (n4 b))) -> p1 (p2 (p3 (p4 c)))) -> IdentityT4 m1 m2 m3 m4 a -> IdentityT4 n1 n2 n3 n4 b -> IdentityT4 p1 p2 p3 p4 c
lift2IdentityT4 f a b = IdentityT4 (f (runIdentityT4 a) (runIdentityT4 b))

mapIdentityT4 :: (m1 (m2 (m3 (m4 a))) -> n1 (n2 (n3 (n4 b)))) -> IdentityT4 m1 m2 m3 m4 a -> IdentityT4 n1 n2 n3 n4 b
mapIdentityT4 f = IdentityT4 . f . runIdentityT4

liftCallCC4 :: CallCC4 m1 m2 m3 m4 a b -> CallCC (IdentityT4 m1 m2 m3 m4) a b
liftCallCC4 callCC f = IdentityT4 $ callCC $ \c -> runIdentityT4 $ (c >-> IdentityT4) >- f

liftCatch4 :: Catch4 e m1 m2 m3 m4 a -> Catch e (IdentityT4 m1 m2 m3 m4) a
liftCatch4 catch m h = IdentityT4 $ (runIdentityT4 m) `catch` (h >-> runIdentityT4)

infixl 3  ****:
(****:) :: (Monad m1, Monad m2, Monad m3, Monad m4) => Identity a -> IdentityT4 m1 m2 m3 m4 a
(****:) = IdentityT4 . (.****) . runIdentity

infixl 3  ---*:, --*-:, -*--:, *---:
(---*:) :: (Monad m1, Monad m2, Monad m3, Monad m4) => IdentityT3 m1 m2 m3 a -> IdentityT4 m1 m2 m3 m4 a
(---*:) = IdentityT4 . (---*) . runIdentityT3
(--*-:) :: (Monad m1, Monad m2, Monad m3, Monad m4) => IdentityT3 m1 m2 m4 a -> IdentityT4 m1 m2 m3 m4 a
(--*-:) = IdentityT4 . (--*) . runIdentityT3
(-*--:) :: (Monad m1, Monad m2, Monad m3, Monad m4) => IdentityT3 m1 m3 m4 a -> IdentityT4 m1 m2 m3 m4 a
(-*--:) = IdentityT4 . (-*) . runIdentityT3
(*---:) :: (Monad m1, Monad m2, Monad m3, Monad m4) => IdentityT3 m2 m3 m4 a -> IdentityT4 m1 m2 m3 m4 a
(*---:) = IdentityT4 . (.*) . runIdentityT3

infixl 3  --**:, -*-*:, -**-:, *-*-:, **--:, *--*:
(--**:) :: (Monad m1, Monad m2, Monad m3, Monad m4) => IdentityT2 m1 m2 a -> IdentityT4 m1 m2 m3 m4 a
(--**:) = (---*:) . (--*:)
(-*-*:) :: (Monad m1, Monad m2, Monad m3, Monad m4) => IdentityT2 m1 m3 a -> IdentityT4 m1 m2 m3 m4 a
(-*-*:) = (---*:) . (-*-:)
(-**-:) :: (Monad m1, Monad m2, Monad m3, Monad m4) => IdentityT2 m1 m4 a -> IdentityT4 m1 m2 m3 m4 a
(-**-:) = (--*-:) . (-*-:)
(*-*-:) :: (Monad m1, Monad m2, Monad m3, Monad m4) => IdentityT2 m2 m4 a -> IdentityT4 m1 m2 m3 m4 a
(*-*-:) = (--*-:) . (*--:)
(**--:) :: (Monad m1, Monad m2, Monad m3, Monad m4) => IdentityT2 m3 m4 a -> IdentityT4 m1 m2 m3 m4 a
(**--:) = (-*--:) . (*--:)
(*--*:) :: (Monad m1, Monad m2, Monad m3, Monad m4) => IdentityT2 m2 m3 a -> IdentityT4 m1 m2 m3 m4 a
(*--*:) = (---*:) . (*--:)

infixl 3  -***:, *-**:, **-*:, ***-: 
(-***:) :: (Monad m1, Monad m2, Monad m3, Monad m4) => IdentityT m1 a -> IdentityT4 m1 m2 m3 m4 a
(-***:) = (---*:) . (-**:)
(*-**:) :: (Monad m1, Monad m2, Monad m3, Monad m4) => IdentityT m2 a -> IdentityT4 m1 m2 m3 m4 a
(*-**:) = (---*:) . (*-*:)
(**-*:) :: (Monad m1, Monad m2, Monad m3, Monad m4) => IdentityT m3 a -> IdentityT4 m1 m2 m3 m4 a
(**-*:) = (---*:) . (**-:)
(***-:) :: (Monad m1, Monad m2, Monad m3, Monad m4) => IdentityT m4 a -> IdentityT4 m1 m2 m3 m4 a
(***-:) = (--*-:) . (**-:)

----------------------------------------------------------------------
-- Level-5

newtype IdentityT5 f1 f2 f3 f4 f5 a = IdentityT5 { runIdentityT5 :: f1 (f2 (f3 (f4 (f5 a)))) }
    deriving (Functor, Eq, Ord, Read, Show, Foldable, Traversable)

instance (Applicative m1, Applicative m2, Applicative m3, Applicative m4, Applicative m5) => Applicative (IdentityT5 m1 m2 m3 m4 m5) where
    pure x = IdentityT5 $ (.*****) x
    (<*>) = lift2IdentityT5 (|*>>>>>)
instance (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => Monad (IdentityT5 m1 m2 m3 m4 m5) where
    return = IdentityT5 . (.*****)
    m >>= f = IdentityT5 $ runIdentityT5 m >>>>>= (f >-> runIdentityT5)

instance (Alternative m1, Alternative m2, Alternative m3, Alternative m4, Alternative m5) => Alternative (IdentityT5 m1 m2 m3 m4 m5) where
    empty = IdentityT5 empty
    (<|>) = lift2IdentityT5 (<|>)
instance (MonadPlus m1, Alternative m2, Monad m2, Traversable m2, Alternative m3, Monad m3, Traversable m3, Alternative m4, Monad m4, Traversable m4, Alternative m5, Monad m5, Traversable m5) => MonadPlus (IdentityT5 m1 m2 m3 m4 m5) where
    mzero = IdentityT5 mzero
    mplus = lift2IdentityT5 mplus

instance (MonadIO m1, Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => MonadIO (IdentityT5 m1 m2 m3 m4 m5) where
    liftIO = IdentityT5 . (-****) . liftIO

transrollI5 :: (Monad m1, Monad (t2 m1), Monad (t3 (t2 m1)), Monad (t4 (t3 (t2 m1))), MonadTrans_ m2 t2, MonadTrans_ m3 t3, MonadTrans_ m4 t4, MonadTrans_ m5 t5) => 
                IdentityT5 m1 m2 m3 m4 m5 a -> IdentityT (t5 (t4 (t3 (t2 m1)))) a
transrollI5 = IdentityT . transroll5 . runIdentityT5
untransrollI5 :: (Monad m1, Monad (t2 m1), Monad (t3 (t2 m1)), Monad (t4 (t3 (t2 m1))), MonadTrans_ m2 t2, MonadTrans_ m3 t3, MonadTrans_ m4 t4, MonadTrans_ m5 t5) => 
                  IdentityT (t5 (t4 (t3 (t2 m1)))) a -> IdentityT5 m1 m2 m3 m4 m5 a
untransrollI5 = IdentityT5 . untransroll5 . runIdentityT

lift2IdentityT5 :: (m1 (m2 (m3 (m4 (m5 a)))) -> n1 (n2 (n3 (n4 (n5 b)))) -> p1 (p2 (p3 (p4 (p5 c))))) -> IdentityT5 m1 m2 m3 m4 m5 a -> IdentityT5 n1 n2 n3 n4 n5 b -> IdentityT5 p1 p2 p3 p4 p5 c
lift2IdentityT5 f a b = IdentityT5 (f (runIdentityT5 a) (runIdentityT5 b))

mapIdentityT5 :: (m1 (m2 (m3 (m4 (m5 a)))) -> n1 (n2 (n3 (n4 (n5 b))))) -> IdentityT5 m1 m2 m3 m4 m5 a -> IdentityT5 n1 n2 n3 n4 n5 b
mapIdentityT5 f = IdentityT5 . f . runIdentityT5

liftCallCC5 :: CallCC5 m1 m2 m3 m4 m5 a b -> CallCC (IdentityT5 m1 m2 m3 m4 m5) a b
liftCallCC5 callCC f = IdentityT5 $ callCC $ \c -> runIdentityT5 $ (c >-> IdentityT5) >- f

liftCatch5 :: Catch5 e m1 m2 m3 m4 m5 a -> Catch e (IdentityT5 m1 m2 m3 m4 m5) a
liftCatch5 catch m h = IdentityT5 $ (runIdentityT5 m) `catch` (h >-> runIdentityT5)

infixl 3  *****:
(*****:) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => Identity a -> IdentityT5 m1 m2 m3 m4 m5 a
(*****:) = IdentityT5 . (.*****) . runIdentity

infixl 3  ----*:, ---*-:, --*--:, -*---:, *----:
(----*:) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT4 m1 m2 m3 m4 a -> IdentityT5 m1 m2 m3 m4 m5 a
(----*:) = IdentityT5 . (----*) . runIdentityT4
(---*-:) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT4 m1 m2 m3 m5 a -> IdentityT5 m1 m2 m3 m4 m5 a
(---*-:) = IdentityT5 . (---*) . runIdentityT4
(--*--:) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT4 m1 m2 m4 m5 a -> IdentityT5 m1 m2 m3 m4 m5 a
(--*--:) = IdentityT5 . (--*) . runIdentityT4
(-*---:) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT4 m1 m3 m4 m5 a -> IdentityT5 m1 m2 m3 m4 m5 a
(-*---:) = IdentityT5 . (-*) . runIdentityT4
(*----:) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT4 m2 m3 m4 m5 a -> IdentityT5 m1 m2 m3 m4 m5 a
(*----:) = IdentityT5 . (.*) . runIdentityT4

infixl 3  ---**:, --*-*:, -*--*:, *---*:, *--*-:, -*-*-:, --**-:, -**--:, *-*--:, **---:
(---**:) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT3 m1 m2 m3 a -> IdentityT5 m1 m2 m3 m4 m5 a
(---**:) = (----*:) . (---*:)
(--*-*:) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT3 m1 m2 m4 a -> IdentityT5 m1 m2 m3 m4 m5 a
(--*-*:) = (----*:) . (--*-:)
(-*--*:) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT3 m1 m3 m4 a -> IdentityT5 m1 m2 m3 m4 m5 a
(-*--*:) = (----*:) . (-*--:)
(*---*:) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT3 m2 m3 m4 a -> IdentityT5 m1 m2 m3 m4 m5 a
(*---*:) = (----*:) . (*---:)
(*--*-:) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT3 m2 m3 m5 a -> IdentityT5 m1 m2 m3 m4 m5 a
(*--*-:) = (---*-:) . (*---:)
(-*-*-:) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT3 m1 m3 m5 a -> IdentityT5 m1 m2 m3 m4 m5 a
(-*-*-:) = (---*-:) . (-*--:)
(--**-:) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT3 m1 m2 m5 a -> IdentityT5 m1 m2 m3 m4 m5 a
(--**-:) = (---*-:) . (--*-:)
(-**--:) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT3 m1 m4 m5 a -> IdentityT5 m1 m2 m3 m4 m5 a
(-**--:) = (--*--:) . (-*--:)
(*-*--:) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT3 m2 m4 m5 a -> IdentityT5 m1 m2 m3 m4 m5 a
(*-*--:) = (*----:) . (-*--:)
(**---:) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT3 m3 m4 m5 a -> IdentityT5 m1 m2 m3 m4 m5 a
(**---:) = (-*---:) . (*---:)
infixl 3  --***:, -*-**:, *--**:, *-*-*:, -**-*:, **--*:, **-*-:, *-**-:, -***-:, ***--:
(--***:) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT2 m1 m2 a -> IdentityT5 m1 m2 m3 m4 m5 a
(--***:) = (----*:) . (--**:)
(-*-**:) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT2 m1 m3 a -> IdentityT5 m1 m2 m3 m4 m5 a
(-*-**:) = (----*:) . (-*-*:)
(*--**:) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT2 m2 m3 a -> IdentityT5 m1 m2 m3 m4 m5 a
(*--**:) = (----*:) . (*--*:)
(*-*-*:) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT2 m2 m4 a -> IdentityT5 m1 m2 m3 m4 m5 a
(*-*-*:) = (----*:) . (*-*-:)
(-**-*:) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT2 m1 m4 a -> IdentityT5 m1 m2 m3 m4 m5 a
(-**-*:) = (----*:) . (-**-:)
(**--*:) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT2 m3 m4 a -> IdentityT5 m1 m2 m3 m4 m5 a
(**--*:) = (----*:) . (**--:)
(**-*-:) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT2 m3 m5 a -> IdentityT5 m1 m2 m3 m4 m5 a
(**-*-:) = (---*-:) . (**--:)
(*-**-:) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT2 m2 m5 a -> IdentityT5 m1 m2 m3 m4 m5 a
(*-**-:) = (*----:) . (-**-:)
(-***-:) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT2 m1 m5 a -> IdentityT5 m1 m2 m3 m4 m5 a
(-***-:) = (---*-:) . (-**-:)
(***--:) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT2 m4 m5 a -> IdentityT5 m1 m2 m3 m4 m5 a
(***--:) = (--*--:) . (**--:)
infixl 3  -****:, *-***:, **-**:, ***-*:, ****-:
(-****:) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT m1 a -> IdentityT5 m1 m2 m3 m4 m5 a
(-****:) = (----*:) . (-***:)
(*-***:) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT m2 a -> IdentityT5 m1 m2 m3 m4 m5 a
(*-***:) = (----*:) . (*-**:)
(**-**:) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT m3 a -> IdentityT5 m1 m2 m3 m4 m5 a
(**-**:) = (----*:) . (**-*:)
(***-*:) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT m4 a -> IdentityT5 m1 m2 m3 m4 m5 a
(***-*:) = (----*:) . (***-:)
(****-:) :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => IdentityT m5 a -> IdentityT5 m1 m2 m3 m4 m5 a
(****-:) = (---*-:) . (***-:)

{- $Example
Here is an example showing how to use identity-cover functions

>import DeepControl.Applicative ((|$>))
>import DeepControl.Monad (Monad)
>import DeepControl.Monad.Morph (generalize, (|*|), (|>|))
>import DeepControl.Monad.Trans.Identity (IdentityT(..), IdentityT2(..), (-*:), (*-:))
>import Control.Monad.Writer
>import Control.Monad.State
>
>tick :: State Int ()
>tick = modify (+1)
>
>tock                         ::                   StateT Int IO ()
>tock = do
>    generalize |>| tick      :: (Monad      m) => StateT Int m  ()  -- (|>|) is the level-1 trans-map function, analogous to (|$>)
>    (|*|) $ putStrLn "Tock!" :: (MonadTrans t) => t          IO ()  -- (|*|) is the level-1 trans-lift function, alias to 'lift'
>
>-- λ> runStateT tock 0
>-- Tock!
>-- ((),1)
>
>save :: StateT Int (Writer [Int]) ()
>save = do
>    n <- get
>    (|*|) $ tell [n]
>
>program ::                             StateT Int (IdentityT2 IO (Writer [Int])) () -- StateT-IdentityT2-IO-Writer monad, a level-2 monad-transform
>program = replicateM_ 4 $ do
>    ((-*:) . IdentityT) |>| tock                                                    -- (-*:) is a level-2 identity-cover function, analogous to (-*)
>        :: (Monad m, Traversable m) => StateT Int (IdentityT2 IO m             ) ()
>    ((*-:) . IdentityT) |>| save                                                    -- (*-:) is a level-2 identity-cover function, analogous to (.*)
>        :: (Monad m               ) => StateT Int (IdentityT2 m  (Writer [Int])) ()
>
>-- λ> execWriter |$> runIdentityT2 (runStateT program 0)
>-- Tock!
>-- Tock!
>-- Tock!
>-- Tock!
>-- [1,2,3,4]
-}
