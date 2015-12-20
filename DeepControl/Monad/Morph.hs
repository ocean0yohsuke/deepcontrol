{-|
Module      : DeepControl.Monad.Morph
Description : Deepened the usual Control.Monad.Morph module.
Copyright   : 2013 Gabriel Gonzalez,
              (c) 2015 KONISHI Yohsuke
License     : BSD-style (see the LICENSE file in the distribution)
Maintainer  : ocean0yohsuke@gmail.com
Stability   : experimental
Portability : ---

This module enables you to program in Monad-Morphic style for much __deeper__ level than the usual @Control.Monad.Morph@ module expresses.
You would realize exactly what __/much deeper level/__ means by reading the example codes, which are attached on the page bottom.
-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module DeepControl.Monad.Morph (
    module Control.Monad.Morph,

    -- * SinkT
    SinkT(..), sinkT2, sinkT3, sinkT4,

    -- * Level-1
    -- ** trans-map
    (|>|), (|<|),
    -- ** trans-sequence
    (|>~),
    -- ** trans-cover
    (|*|),
    -- ** trans-fish
    (|>=>),

    -- * Level-2
    -- ** trans-bind
    (|>>=),
    -- ** trans-sequence
    (|>>~),
    -- ** trans-fish
    (|>>=>),
    -- ** trans-map
    (|>>|), (|<<|),
    -- ** trans-cover
    (|**|),
    (|-*|),

    -- * Level-3
    -- ** trans-bind
    (|>>>=),
    -- ** trans-sequence
    (|>>>~),
    -- ** trans-fish
    (|>>>=>),
     -- ** trans-map
    (|>>>|), (|<<<|),
    -- ** trans-cover
    (|***|),
    (|--*|),
    (|-**|), (|*-*|),

    -- * Level-4
    -- ** trans-bind
    (|>>>>=),
    -- ** trans-sequence
    (|>>>>~),
    -- ** trans-map
    (|>>>>|), (|<<<<|),
    -- ** trans-cover
    (|****|),
    (|---*|),
    (|--**|), (|-*-*|), (|*--*|),
    (|-***|), (|*-**|), (|**-*|),

    -- * Level-5
    -- ** trans-bind
    (|>>>>>=),
    -- ** trans-sequence
    (|>>>>>~),
    -- ** trans-map
    (|>>>>>|), (|<<<<<|),
    -- ** trans-cover
    (|*****|),
    (|----*|),
    (|---**|), (|--*-*|), (|-*--*|), (|*---*|),
    (|--***|), (|-*-**|), (|*--**|), (|*-*-*|), (|-**-*|), (|**--*|),
    (|-****|), (|*-***|), (|**-**|), (|***-*|),

    -- * Level-2 example: trans-map
    -- $Example

    -- * Level-2 example: trans-cover and trans-bind
    -- $Example-2

    ) where

import           DeepControl.Applicative
import           DeepControl.Monad.Trans
import           DeepControl.Traversable

import           Control.Monad.Except         (Except, ExceptT (..), runExcept,
                                               runExceptT)
import           Control.Monad.Identity       (Identity (..))
import           Control.Monad.List           (ListT (..))
import           Control.Monad.Morph
import           Control.Monad.Trans.Identity (IdentityT (..))
import           Control.Monad.Trans.Maybe    (MaybeT (..))
import           Control.Monad.Writer         (Writer, WriterT (..), runWriter)
import           Data.Monoid

-------------------------------------------------------------------------------
-- SinkT

class (MonadTrans s) => SinkT s where
    -- | Alalog to @'DeepControl.Traversable.sink'@.
    --
    -- >>> sinkT $ MaybeT (ListT (Right [Just 1]))
    -- ListT (MaybeT (Right (Just [1])))
    --
    -- >>> sinkT $ MaybeT (ListT (ExceptT (Identity (Right [Just 1]))))
    -- ListT (MaybeT (ExceptT (Identity (Right (Just [1])))))
    --
    sinkT :: (Monad m, MMonad t,
              MonadTrans_ x t, Traversable x) =>
             s (t m) a -> t (s m) a

instance SinkT IdentityT where
    sinkT (IdentityT x) = trans . IdentityT . untrans $ x
instance SinkT MaybeT where
    sinkT (MaybeT x)    = trans . MaybeT . (sink|$>) . untrans $ x
instance SinkT ListT where
    sinkT (ListT x)     = trans . ListT . (sink|$>) . untrans $ x
instance SinkT (ExceptT e) where
    sinkT (ExceptT x)   = trans . ExceptT . (sink|$>) . untrans $ x
instance (Monoid w) => SinkT (WriterT w) where
    sinkT (WriterT x)   = trans . WriterT . (sinkToTuple|$>) . untrans $ x
      where
        sinkToTuple :: (Functor m, Traversable m, Monoid w) => m (a, w) -> (m a, w)
        sinkToTuple = flipTuple . sink . (flipTuple|$>)
        flipTuple (x,y) = (y,x)

-- | Alalog to @'DeepControl.Traversable.sink2'@.
--
-- >>> sinkT2 $ MaybeT (ListT (ExceptT (Identity (Right [Just 1]))))
-- ListT (ExceptT (MaybeT (Identity (Just (Right [1])))))
--
sinkT2 :: (Monad m, Monad (s (t2 m)), Monad (t2 m),
           MonadTrans_ x1 t1, Traversable x1, MonadTrans_ x2 t2, Traversable x2,
           SinkT s, MMonad t1, MMonad t2) =>
          s (t1 (t2 m)) a -> t1 (t2 (s m)) a
sinkT2 = (sinkT|>|) . sinkT

sinkT3
  :: (Monad m, Monad (s (t2 (t3 m))), Monad (s (t3 m)), Monad (t2 (t3 m)), Monad (t3 m),
      MonadTrans_ x1 t1, Traversable x1, MonadTrans_ x2 t2, Traversable x2, MonadTrans_ x3 t3, Traversable x3,
      SinkT s, MMonad t1, MMonad t2, MMonad t3) =>
     s (t1 (t2 (t3 m))) a -> t1 (t2 (t3 (s m))) a
sinkT3 = (sinkT2|>|) . sinkT

sinkT4
  :: (Monad m, Monad (s (t2 (t3 (t4 m)))), Monad (s (t3 (t4 m))), Monad (s (t4 m)), Monad (t2 (t3 (t4 m))), Monad (t3 (t4 m)), Monad (t4 m),
      MonadTrans_ x1 t1, Traversable x1, MonadTrans_ x2 t2, Traversable x2, MonadTrans_ x3 t3, Traversable x3, MonadTrans_ x4 t4, Traversable x4,
      MMonad t1, MMonad t2, MMonad t3, MMonad t4, SinkT s) =>
     s (t1 (t2 (t3 (t4 m)))) a -> t1 (t2 (t3 (t4 (s m)))) a
sinkT4 = (sinkT3|>|) . sinkT

-------------------------------------------------------------------------------
-- Level-1 functions

infixl 4  |>|
-- | Alias to @'hoist'@.
(|>|) :: (Monad m, MFunctor t) => (forall a . m a -> n a) -> t m b -> t n b
(|>|) = hoist

infixl 4  |<|
-- | Equivalent to (|>|) with the arguments flipped.
(|<|) :: (Monad m, MFunctor t) => t m b -> (forall a . m a -> n a) -> t n b
(|<|) l r = hoist r l

infixl 5 |*|
-- | Alias to @'lift'@
(|*|) :: (Monad m, MonadTrans t) => m a -> t m a
(|*|) = lift

infixr 3  |>=>
(|>=>) :: (Monad m3, MMonad t) => (forall a. m1 a -> t m2 a) -> (forall b. m2 b -> t m3 b) -> m1 c -> t m3 c
(|>=>) = (>|>)

infixr 3  |>~
(|>~) :: (Monad n, MMonad t1) => t1 m b -> (forall a. t1 n a) -> t1 n b
m |>~ k = m |>= \_ -> k

-------------------------------------------------------------------------------
-- Level-2 functions

infixr 3  |>>=
-- | The level-2 trans-bind function, analogous to ('>>=')
(|>>=) :: (Monad n, Monad m, Monad (t2 n), Monad (t2 (t2 n)),
           MonadTrans_ x t1, Traversable x,
           MMonad t1, MMonad t2, SinkT t2) =>
          t1 (t2 m) b -> (forall a. m a -> t1 (t2 n) a) -> t1 (t2 n) b
m |>>= f = m |>= \x -> squash |>| (sinkT $ f |>| x)

infixr 3  |>>~
(|>>~) :: (Monad n, Monad m, Monad (t2 n), Monad (t2 (t2 n)),
           MonadTrans_ x t1, Traversable x,
           MMonad t1, MMonad t2, SinkT t2) =>
          t1 (t2 m) b -> (forall a. t1 (t2 n) a) -> t1 (t2 n) b
m |>>~ k = m |>>= \_ -> k

infixr 3  |>>=>
(|>>=>) :: (Monad m3, Monad m2, Monad (t2 m3), Monad (t2 (t2 m3)),
            MonadTrans_ x t1, Traversable x,
            MMonad t1, MMonad t2, SinkT t2) =>
           (forall a. m1 a -> t1 (t2 m2) a) -> (forall b. m2 b -> t1 (t2 m3) b) -> m1 c -> t1 (t2 m3) c
f |>>=> g = \x -> f x |>>= g

infixl 4  |>>|
(|>>|) :: (Monad m, Monad (t2 m), MFunctor t1, MFunctor t2) =>
          (forall a . m a -> n a) -> t1 (t2 m) b -> t1 (t2 n) b
(|>>|) f g = (f |>|) |>| g

infixl 4  |<<|
(|<<|) :: (Monad m, Monad (t2 m), MFunctor t1, MFunctor t2) =>
           t1 (t2 m) b -> (forall a . m a -> n a) -> t1 (t2 n) b
(|<<|) f g = (g |>|) |>| f

infixl 5 |**|
(|**|) :: (Monad m, MonadTrans t1, MonadTrans t2, Monad (t2 m)) => m a -> t1 (t2 m) a
(|**|) = (|*|) . (|*|)

infixl 5  |-*|
(|-*|) :: (Monad m, MonadTrans t1, MonadTrans t2, MFunctor t1) => t1 m a -> t1 (t2 m) a
(|-*|) = ((|*|) |>|)

-------------------------------------------------------------------------------
-- Level-3 functions

infixr 3  |>>>=
(|>>>=) ::
   (Monad n, Monad (t3 n), Monad m, Monad (t3 m),
    Monad (t2 (t3 n)), Monad (t2 (t3 (t3 n))), Monad (t3 (t3 n)), Monad (t3 (t2 (t3 n))), Monad (t2 (t2 (t3 n))),
    MonadTrans_ x1 t1, Traversable x1, MonadTrans_ x2 t2, Traversable x2,
    SinkT t2, SinkT t3,
    MMonad t1, MMonad t2, MMonad t3) =>
   t1 (t2 (t3 m)) b -> (forall a. m a -> t1 (t2 (t3 n)) a) -> t1 (t2 (t3 n)) b
m |>>>= f = m |>>= \x -> squash |>>| (sinkT2 $ f |>| x)

infixr 3  |>>>~
(|>>>~) ::
   (Monad n, Monad (t3 n), Monad m, Monad (t3 m),
    Monad (t2 (t3 n)), Monad (t2 (t3 (t3 n))), Monad (t3 (t3 n)), Monad (t3 (t2 (t3 n))), Monad (t2 (t2 (t3 n))),
    MonadTrans_ x1 t1, Traversable x1, MonadTrans_ x2 t2, Traversable x2,
    SinkT t2, SinkT t3,
    MMonad t1, MMonad t2, MMonad t3) =>
   t1 (t2 (t3 m)) b -> (forall a. t1 (t2 (t3 n)) a) -> t1 (t2 (t3 n)) b
m |>>>~ k = m |>>>= \_ -> k

infixr 3  |>>>=>
(|>>>=>) :: (Monad m3, Monad m2, Monad (t2 m3), Monad (t2 (t2 m3)), Monad (t2 (t2 (t3 m3))), Monad (t2 (t3 m3)), Monad (t2 (t3 (t3 m3))),
             Monad (t3 m3), Monad (t3 m2), Monad (t3 (t2 (t3 m3))), Monad (t3 (t3 m3)),
             MonadTrans_ x1 t1, Traversable x1, MonadTrans_ x2 t2, Traversable x2,
             MMonad t1, MMonad t2, MMonad t3, SinkT t2, SinkT t3) =>
            (forall a. m1 a -> t1 (t2 (t3 m2)) a) -> (forall b. m2 b -> t1 (t2 (t3 m3)) b) -> m1 c -> t1 (t2 (t3 m3)) c
f |>>>=> g = \x -> f x |>>>= g

infixl 4  |>>>|
(|>>>|) :: (Monad m, Monad (t3 m), Monad (t2 (t3 m)), MFunctor t1, MFunctor t2, MFunctor t3) =>
           (forall a . m a -> n a) -> t1 (t2 (t3 m)) b -> t1 (t2 (t3 n)) b
(|>>>|) f g = (f |>|) |>>| g

infixl 4  |<<<|
(|<<<|) :: (Monad m, Monad (t3 m), Monad (t2 (t3 m)), MFunctor t1, MFunctor t2, MFunctor t3) =>
           t1 (t2 (t3 m)) b -> (forall a . m a -> n a) -> t1 (t2 (t3 n)) b
(|<<<|) f g = (g |>|) |>>| f

infixl 5 |***|
(|***|) :: (Monad m, Monad (t2 (t3 m)), Monad (t3 m),
            MonadTrans t1, MonadTrans t2, MonadTrans t3) =>
            m a -> t1 (t2 (t3 m)) a
(|***|) = (|*|) . (|**|)

infixl 5  |--*|
(|--*|) :: (Monad m, Monad (t2 m),
            MonadTrans t1, MonadTrans t2, MonadTrans t3,
            MFunctor t1, MFunctor t2) =>
            t1 (t2 m) a -> t1 (t2 (t3 m)) a
(|--*|) = ((|*|) |>>|)

infixl 5  |-**|, |*-*|
(|-**|) :: (Monad m, Monad (t2 (t3 m)), Monad (t3 m),
            MonadTrans t1, MonadTrans t2, MonadTrans t3,
            MFunctor t1) =>
            t1 m a -> t1 (t2 (t3 m)) a
(|-**|) = ((|**|) |>|)
(|*-*|) :: (Monad m, Monad (t3 m), Monad (t2 (t3 m)),
            MonadTrans t1, MonadTrans t2, MonadTrans t3,
            MFunctor t2) =>
            t2 m a -> t1 (t2 (t3 m)) a
(|*-*|) = (|*|) . ((|*|) |>|)

-------------------------------------------------------------------------------
-- Level-4 functions

infixr 4  |>>>>=
(|>>>>=) ::
   (Monad n, Monad (t4 n), Monad (t4 m), Monad m,
    Monad (t2 (t3 (t4 n))), Monad (t2 (t3 (t4 (t4 n)))), Monad (t2 (t2 (t3 (t4 n)))),
    Monad (t3 (t4 n)), Monad (t3 (t4 (t4 n))), Monad (t4 (t4 n)), Monad (t4 (t2 (t3 (t4 n)))), Monad (t2 (t3 (t3 (t4 n)))), Monad (t4 (t3 (t4 n))),
    Monad (t2 (t4 n)), Monad (t3 (t2 (t3 (t4 n)))), Monad (t3 (t3 (t4 n))), Monad (t3 (t4 m)),
    MonadTrans_ x1 t1, Traversable x1, MonadTrans_ x2 t2, Traversable x2, MonadTrans_ x3 t3, Traversable x3,
    SinkT t2, SinkT t3, SinkT t4,
    MMonad t1, MMonad t2, MMonad t3, MMonad t4) =>
   t1 (t2 (t3 (t4 m))) b -> (forall a. m a -> t1 (t2 (t3 (t4 n))) a) -> t1 (t2 (t3 (t4 n))) b
m |>>>>= f = m |>>>= \x -> squash |>>>| (sinkT3 $ f |>| x)

infixr 3  |>>>>~
(|>>>>~) ::
   (Monad n, Monad (t4 n), Monad (t4 m), Monad m,
    Monad (t2 (t3 (t4 n))), Monad (t2 (t3 (t4 (t4 n)))), Monad (t2 (t2 (t3 (t4 n)))),
    Monad (t3 (t4 n)), Monad (t3 (t4 (t4 n))), Monad (t4 (t4 n)), Monad (t4 (t2 (t3 (t4 n)))), Monad (t2 (t3 (t3 (t4 n)))), Monad (t4 (t3 (t4 n))),
    Monad (t2 (t4 n)), Monad (t3 (t2 (t3 (t4 n)))), Monad (t3 (t3 (t4 n))), Monad (t3 (t4 m)),
    MonadTrans_ x1 t1, Traversable x1, MonadTrans_ x2 t2, Traversable x2, MonadTrans_ x3 t3, Traversable x3,
    SinkT t2, SinkT t3, SinkT t4,
    MMonad t1, MMonad t2, MMonad t3, MMonad t4) =>
   t1 (t2 (t3 (t4 m))) b -> (forall a. t1 (t2 (t3 (t4 n))) a) -> t1 (t2 (t3 (t4 n))) b
m |>>>>~ k = m |>>>>= \_ -> k

infixl 4  |>>>>|
(|>>>>|) :: (Monad m, Monad (t4 m), Monad (t3 (t4 m)), Monad (t2 (t3 (t4 m))), MFunctor t1, MFunctor t2, MFunctor t3, MFunctor t4) =>
            (forall a . m a -> n a) -> t1 (t2 (t3 (t4 m))) b -> t1 (t2 (t3 (t4 n))) b
(|>>>>|) f g = (f |>|) |>>>| g

infixl 4  |<<<<|
(|<<<<|) :: (Monad m, Monad (t4 m), Monad (t3 (t4 m)), Monad (t2 (t3 (t4 m))), MFunctor t1, MFunctor t2, MFunctor t3, MFunctor t4) =>
            t1 (t2 (t3 (t4 m))) b -> (forall a . m a -> n a) -> t1 (t2 (t3 (t4 n))) b
(|<<<<|) f g = (g |>|) |>>>| f

infixl 5 |****|
(|****|) :: (Monad m, Monad (t2 (t3 (t4 m))), Monad (t3 (t4 m)), Monad (t4 m),
            MonadTrans t1, MonadTrans t2, MonadTrans t3, MonadTrans t4) =>
            m a -> t1 (t2 (t3 (t4 m))) a
(|****|) = (|*|) . (|***|)

infixl 5  |---*|
(|---*|) :: (Monad m, Monad (t2 (t3 m)), Monad (t3 m),
            MFunctor t1, MFunctor t2, MFunctor t3,
            MonadTrans t1, MonadTrans t2, MonadTrans t3, MonadTrans t4) =>
            t1 (t2 (t3 m)) a -> t1 (t2 (t3 (t4 m))) a
(|---*|) = ((|*|) |>>>|)
infixl 5  |--**|, |-*-*|
(|--**|) :: (Monad m, Monad (t2 m), Monad (t4 m),
            MFunctor t1, MFunctor t2,
            MonadTrans t1, MonadTrans t2, MonadTrans t3, MonadTrans t4) =>
            t1 (t2 m) a -> t1 (t2 (t3 (t4 m))) a
(|--**|) = ((|**|) |>>|)
(|-*-*|) :: (Monad m, Monad (t3 m), Monad (t3 (t4 m)), Monad (t4 m),
            MFunctor t1, MFunctor t3,
            MonadTrans t1, MonadTrans t2, MonadTrans t3, MonadTrans t4) =>
            t1 (t3 m) a -> t1 (t2 (t3 (t4 m))) a
(|-*-*|) = ((|*-*|) |>|)
(|*--*|) :: (Monad m, Monad (t3 m), Monad (t2 (t3 (t4 m))), Monad (t2 (t3 m)),
            MFunctor t2, MFunctor t3,
            MonadTrans t1, MonadTrans t2, MonadTrans t3, MonadTrans t4) =>
            t2 (t3 m) a -> t1 (t2 (t3 (t4 m))) a
(|*--*|) = (|*|) . (|--*|)
infixl 5  |-***|, |*-**|, |**-*|
(|-***|) :: (Monad m, Monad (t3 (t4 m)), Monad (t4 m),
            MFunctor t1,
            MonadTrans t1, MonadTrans t2, MonadTrans t3, MonadTrans t4) =>
            t1 m a -> t1 (t2 (t3 (t4 m))) a
(|-***|) = ((|***|) |>|)
(|*-**|) :: (Monad m, Monad (t2 (t3 (t4 m))), Monad (t3 (t4 m)), Monad (t4 m),
            MFunctor t2,
            MonadTrans t1, MonadTrans t2, MonadTrans t3, MonadTrans t4) =>
            t2 m a -> t1 (t2 (t3 (t4 m))) a
(|*-**|) = (|*|) . (|-**|)
(|**-*|) :: (Monad m, Monad (t2 (t3 (t4 m))), Monad (t3 (t4 m)),
            MFunctor t3,
            MonadTrans t1, MonadTrans t2, MonadTrans t3, MonadTrans t4) =>
            t3 m a -> t1 (t2 (t3 (t4 m))) a
(|**-*|) = (|**|) . (|-*|)

-------------------------------------------------------------------------------
-- Level-5 functions

infixr 4  |>>>>>=
(|>>>>>=) ::
   (Monad n, Monad m, Monad (t5 n), Monad (t5 m),
    Monad (t2 (t3 (t4 (t5 n)))), Monad (t2 (t3 (t4 (t5 (t5 n))))), Monad (t3 (t4 (t5 n))), Monad (t3 (t4 (t5 (t5 n)))),
    Monad (t4 (t5 n)), Monad (t4 (t5 (t5 n))), Monad (t5 (t2 (t3 (t4 (t5 n))))), Monad (t5 (t5 n)), Monad (t5 (t3 (t4 (t5 n)))),
    Monad (t2 (t2 (t3 (t4 (t5 n))))), Monad (t5 (t4 (t5 n))), Monad (t2 (t3 (t3 (t4 (t5 n))))), Monad (t2 (t3 (t4 (t4 (t5 n))))),
    Monad (t2 (t4 (t5 n))), Monad (t3 (t2 (t3 (t4 (t5 n))))), Monad (t3 (t3 (t4 (t5 n)))), Monad (t3 (t4 (t4 (t5 n)))),
    Monad (t3 (t4 (t5 m))), Monad (t4 (t2 (t3 (t4 (t5 n))))), Monad (t4 (t3 (t4 (t5 n)))), Monad (t4 (t4 (t5 n))), Monad (t4 (t5 m)),
    MonadTrans_ x1 t1, Traversable x1, MonadTrans_ x2 t2, Traversable x2, MonadTrans_ x3 t3, Traversable x3, MonadTrans_ x4 t4, Traversable x4,
    SinkT t2, SinkT t3, SinkT t4, SinkT t5,
    MMonad t1, MMonad t2, MMonad t3, MMonad t4, MMonad t5) =>
   t1 (t2 (t3 (t4 (t5 m)))) b -> (forall a. m a -> t1 (t2 (t3 (t4 (t5 n)))) a) -> t1 (t2 (t3 (t4 (t5 n)))) b
m |>>>>>= f = m |>>>>= \x -> squash |>>>>| (sinkT4 $ f |>| x)

infixr 3  |>>>>>~
(|>>>>>~) ::
   (Monad n, Monad m, Monad (t5 n), Monad (t5 m),
    Monad (t2 (t3 (t4 (t5 n)))), Monad (t2 (t3 (t4 (t5 (t5 n))))), Monad (t3 (t4 (t5 n))), Monad (t3 (t4 (t5 (t5 n)))),
    Monad (t4 (t5 n)), Monad (t4 (t5 (t5 n))), Monad (t5 (t2 (t3 (t4 (t5 n))))), Monad (t5 (t5 n)), Monad (t5 (t3 (t4 (t5 n)))),
    Monad (t2 (t2 (t3 (t4 (t5 n))))), Monad (t5 (t4 (t5 n))), Monad (t2 (t3 (t3 (t4 (t5 n))))), Monad (t2 (t3 (t4 (t4 (t5 n))))),
    Monad (t2 (t4 (t5 n))), Monad (t3 (t2 (t3 (t4 (t5 n))))), Monad (t3 (t3 (t4 (t5 n)))), Monad (t3 (t4 (t4 (t5 n)))),
    Monad (t3 (t4 (t5 m))), Monad (t4 (t2 (t3 (t4 (t5 n))))), Monad (t4 (t3 (t4 (t5 n)))), Monad (t4 (t4 (t5 n))), Monad (t4 (t5 m)),
    MonadTrans_ x1 t1, Traversable x1, MonadTrans_ x2 t2, Traversable x2, MonadTrans_ x3 t3, Traversable x3, MonadTrans_ x4 t4, Traversable x4,
    SinkT t2, SinkT t3, SinkT t4, SinkT t5,
    MMonad t1, MMonad t2, MMonad t3, MMonad t4, MMonad t5) =>
   t1 (t2 (t3 (t4 (t5 m)))) b -> (forall a. t1 (t2 (t3 (t4 (t5 n)))) a) -> t1 (t2 (t3 (t4 (t5 n)))) b
m |>>>>>~ k = m |>>>>>= \_ -> k

infixl 4  |>>>>>|
(|>>>>>|) :: (Monad m, Monad (t5 m), Monad (t4 (t5 m)), Monad (t3 (t4 (t5 m))), Monad (t2 (t3 (t4 (t5 m)))), MFunctor t1, MFunctor t2, MFunctor t3, MFunctor t4, MFunctor t5) =>
             (forall a . m a -> n a) -> t1 (t2 (t3 (t4 (t5 m)))) b -> t1 (t2 (t3 (t4 (t5 n)))) b
(|>>>>>|) f g = (f |>|) |>>>>| g

infixl 4  |<<<<<|
(|<<<<<|) :: (Monad m, Monad (t5 m), Monad (t4 (t5 m)), Monad (t3 (t4 (t5 m))), Monad (t2 (t3 (t4 (t5 m)))), MFunctor t1, MFunctor t2, MFunctor t3, MFunctor t4, MFunctor t5) =>
             t1 (t2 (t3 (t4 (t5 m)))) b -> (forall a . m a -> n a) -> t1 (t2 (t3 (t4 (t5 n)))) b
(|<<<<<|) f g = (g |>|) |>>>>| f

infixl 5 |*****|
(|*****|) :: (Monad m, Monad (t2 (t3 (t4 (t5 m)))), Monad (t3 (t4 (t5 m))), Monad (t4 (t5 m)), Monad (t5 m),
              MonadTrans t1, MonadTrans t2, MonadTrans t3, MonadTrans t4, MonadTrans t5) =>
              m a -> t1 (t2 (t3 (t4 (t5 m)))) a
(|*****|) = (|*|) . (|****|)

infixl 5  |----*|
(|----*|) :: (Monad m, Monad (t2 (t3 (t4 m))), Monad (t3 (t4 m)), Monad (t4 m),
              MFunctor t1, MFunctor t2, MFunctor t3, MFunctor t4,
              MonadTrans t1, MonadTrans t2, MonadTrans t3, MonadTrans t4, MonadTrans t5) =>
              t1 (t2 (t3 (t4 m))) a -> t1 (t2 (t3 (t4 (t5 m)))) a
(|----*|) = ((|*|) |>>>>|)

infixl 5  |---**|, |--*-*|, |-*--*|, |*---*|
(|---**|) :: (Monad m, Monad (t2 (t3 m)), Monad (t3 m), Monad (t5 m),
              MFunctor t1, MFunctor t2, MFunctor t3,
              MonadTrans t1, MonadTrans t2, MonadTrans t3, MonadTrans t4, MonadTrans t5) =>
              t1 (t2 (t3 m)) a -> t1 (t2 (t3 (t4 (t5 m)))) a
(|---**|) = ((|**|) |>>>|)
(|--*-*|) :: (Monad m, Monad (t2 (t4 m)), Monad (t4 m), Monad (t4 (t5 m)), Monad (t5 m),
              MFunctor t1, MFunctor t2, MFunctor t4,
              MonadTrans t1, MonadTrans t2, MonadTrans t3, MonadTrans t4, MonadTrans t5) =>
              t1 (t2 (t4 m)) a -> t1 (t2 (t3 (t4 (t5 m)))) a
(|--*-*|) = ((|*-*|) |>>|)
(|-*--*|) :: (Monad m, Monad (t3 (t4 m)), Monad (t4 m), Monad (t3 (t4 (t5 m))),
              MFunctor t1, MFunctor t3, MFunctor t4,
              MonadTrans t1, MonadTrans t2, MonadTrans t3, MonadTrans t4, MonadTrans t5) =>
              t1 (t3 (t4 m)) a -> t1 (t2 (t3 (t4 (t5 m)))) a
(|-*--*|) = ((|*--*|) |>|)
(|*---*|) :: (Monad m, Monad (t3 (t4 m)), Monad (t4 m), Monad (t2 (t3 (t4 (t5 m)))),
              MFunctor t2, MFunctor t3, MFunctor t4,
              MonadTrans t1, MonadTrans t2, MonadTrans t3, MonadTrans t4, MonadTrans t5) =>
              t2 (t3 (t4 m)) a -> t1 (t2 (t3 (t4 (t5 m)))) a
(|*---*|) = (|*|) . (|---*|)

infixl 5  |--***|, |-*-**|, |*--**|, |*-*-*|, |-**-*|, |**--*|
(|--***|) :: (Monad m, Monad (t2 m), Monad (t4 (t5 m)), Monad (t5 m),
              MFunctor t1, MFunctor t2,
              MonadTrans t1, MonadTrans t2, MonadTrans t3, MonadTrans t4, MonadTrans t5) =>
              t1 (t2 m) a -> t1 (t2 (t3 (t4 (t5 m)))) a
(|--***|) = ((|***|) |>>|)
(|-*-**|) :: (Monad m, Monad (t3 m), Monad (t3 (t4 (t5 m))), Monad (t4 (t5 m)), Monad (t5 m),
              MFunctor t1, MFunctor t3,
              MonadTrans t1, MonadTrans t2, MonadTrans t3, MonadTrans t4, MonadTrans t5) =>
              t1 (t3 m) a -> t1 (t2 (t3 (t4 (t5 m)))) a
(|-*-**|) = ((|*-**|) |>|)
(|*--**|) :: (Monad m, Monad (t3 m), Monad (t2 (t3 (t4 (t5 m)))), Monad (t5 m),
              MFunctor t2, MFunctor t3,
              MonadTrans t1, MonadTrans t2, MonadTrans t3, MonadTrans t4, MonadTrans t5) =>
              t2 (t3 m) a -> t1 (t2 (t3 (t4 (t5 m)))) a
(|*--**|) = (|*|) . (|--**|)
(|*-*-*|) :: (Monad m, Monad (t4 m), Monad (t2 (t3 (t4 (t5 m)))), Monad (t4 (t5 m)), Monad (t5 m),
              MFunctor t2, MFunctor t4,
              MonadTrans t1, MonadTrans t2, MonadTrans t3, MonadTrans t4, MonadTrans t5) =>
              t2 (t4 m) a -> t1 (t2 (t3 (t4 (t5 m)))) a
(|*-*-*|) = (|*|) . (|-*-*|)
(|-**-*|) :: (Monad m, Monad (t4 m), Monad (t3 (t4 (t5 m))), Monad (t4 (t5 m)), Monad (t5 m),
              MFunctor t1, MFunctor t4,
              MonadTrans t1, MonadTrans t2, MonadTrans t3, MonadTrans t4, MonadTrans t5) =>
              t1 (t4 m) a -> t1 (t2 (t3 (t4 (t5 m)))) a
(|-**-*|) = (|-*|) . (|-*-*|)
(|**--*|) :: (Monad m, Monad (t4 m), Monad (t2 (t3 (t4 (t5 m)))), Monad (t3 (t4 (t5 m))), Monad (t3 (t4 m)),
              MFunctor t3, MFunctor t4,
              MonadTrans t1, MonadTrans t2, MonadTrans t3, MonadTrans t4, MonadTrans t5) =>
              t3 (t4 m) a -> t1 (t2 (t3 (t4 (t5 m)))) a
(|**--*|) = (|*|) . (|*--*|)

infixl 5  |-****|, |*-***|, |**-**|, |***-*|
(|-****|) :: (Monad m, Monad (t3 (t4 (t5 m))), Monad (t4 (t5 m)), Monad (t5 m),
              MFunctor t1,
              MonadTrans t1, MonadTrans t2, MonadTrans t3, MonadTrans t4, MonadTrans t5) =>
              t1 m a -> t1 (t2 (t3 (t4 (t5 m)))) a
(|-****|) = ((|****|) |>|)
(|*-***|) :: (Monad m, Monad (t2 (t3 (t4 (t5 m)))), Monad (t4 (t5 m)), Monad (t5 m),
              MFunctor t2,
              MonadTrans t1, MonadTrans t2, MonadTrans t3, MonadTrans t4, MonadTrans t5) =>
              t2 m a -> t1 (t2 (t3 (t4 (t5 m)))) a
(|*-***|) = (|*|) . (|-***|)
(|**-**|) :: (Monad m, Monad (t2 (t3 (t4 (t5 m)))), Monad (t3 (t4 (t5 m))), Monad (t4 (t5 m)), Monad (t5 m),
              MFunctor t3,
              MonadTrans t1, MonadTrans t2, MonadTrans t3, MonadTrans t4, MonadTrans t5) =>
              t3 m a -> t1 (t2 (t3 (t4 (t5 m)))) a
(|**-**|) = (|**|) . (|-**|)
(|***-*|) :: (Monad m, Monad (t2 (t3 (t4 (t5 m)))), Monad (t3 (t4 (t5 m))), Monad (t4 (t5 m)),
              MFunctor t4,
              MonadTrans t1, MonadTrans t2, MonadTrans t3, MonadTrans t4, MonadTrans t5) =>
              t4 m a -> t1 (t2 (t3 (t4 (t5 m)))) a
(|***-*|) = (|***|) . (|-*|)

----------------------------------------------------------------------
-- Examples

{- $Example
Here is a monad morph example how to use trans-map functions.

>import DeepControl.Monad.Morph
>import DeepControl.Monad.Trans.State
>import DeepControl.Monad.Trans.Writer
>
>-- i.e. :: StateT Int Identity ()
>tick    :: State Int ()
>tick = modify (+1)
>
>tock                         ::                   StateT Int IO ()
>tock = do
>    generalize |>| tick      :: (Monad      m) => StateT Int m  ()  -- (|>|) is the level-1 trans-map function, analogous to (|$>)
>    (|*|) $ putStrLn "Tock!" :: (MonadTrans t) => t          IO ()
>
>-- λ> runStateT tock 0
>-- Tock!
>-- ((),1)
>
>-- i.e. :: StateT Int (WriterT [Int] Identity) ()
>save    :: StateT Int (Writer  [Int]) ()
>save = do
>    n <- get
>    (|*|) $ tell [n]
>
>program ::                   StateT Int (WriterT [Int] IO) ()
>program = replicateM_ 4 $ do
>    (|*|) |>| tock
>        :: (MonadTrans t) => StateT Int (t             IO) ()
>    generalize |>>| save                                         -- (|>>|) is the level-2 trans-map function, analogous to (|$>>)
>        :: (Monad      m) => StateT Int (WriterT [Int] m ) ()
>
>-- λ> execWriterT (runStateT program 0)
>-- Tock!
>-- Tock!
>-- Tock!
>-- Tock!
>-- [1,2,3,4]
-}

{- $Example-2
Here is a monad morph example how to use trans-cover and trans-bind functions.

>import DeepControl.Monad ((>-))
>import DeepControl.Monad.Morph ((|>=), (|>>=), (|*|), (|-*|))
>import DeepControl.Monad.Trans.Except
>
>import Control.Exception (IOException, try)
>import Control.Monad.Trans.Maybe
>
>-----------------------------------------------
>-- Level-1
>
>check :: IO a ->
>                ExceptT IOException IO a   -- ExceptT-IO monad
>check io = ExceptT $ (try io)
>
>viewFile :: IO ()                          -- IO monad
>viewFile = do
>    str <- readFile "test.txt"
>    putStr str
>
>program :: ExceptT IOException IO ()       -- ExceptT-IO monad
>program = (|*|) viewFile |>= check         -- (|*|) is the level-1 trans-cover function, alias to 'lift' and analogous to (.*)
>                                           -- (|>=) is the level-1 trans-bind function, analogous to (>>=)
>
>calc_program :: IO (Either IOException ())
>calc_program = runExceptT $ program
>
>-- > calc_program
>-- Left test.txt: openFile: does not exist (No such file or directory)
>
>-----------------------------------------------
>-- Level-2
>
>viewFile2 :: String ->
>             MaybeT IO ()                        -- MaybeT-IO monad
>viewFile2 filename = do
>    guard (filename /= "")
>    str <- (|*|) $ readFile filename
>    (|*|) $ putStr str
>
>program2 :: String ->
>            (ExceptT IOException (MaybeT IO)) () -- ExceptT-MaybeT-IO monad
>program2 filename =
>    (|*|) (viewFile2 filename) |>>= \x ->        -- (|>>=) is the level-2 trans-bind function, analogous to (>>=)
>    (|-*|) $ check x                             -- (|-*|) is a level-2 trans-cover function, analogous to (-*)
>
>calc_program2 :: String -> IO (Maybe (Either IOException ()))
>calc_program2 filename = runMaybeT . runExceptT $ program2 filename
>
>-- > calc_program2 "test.txt"
>-- Just (Left test.txt: openFile: does not exist (No such file or directory))
>-- > calc_program2 ""
>-- Nothing
-}
