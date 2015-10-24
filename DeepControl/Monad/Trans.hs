{-|
Module      : DeepControl.Monad.Trans
Description : Enable deep level Monad-Transform programming.
Copyright   : (c) Andy Gill 2001,
              (c) Oregon Graduate Institute of Science and Technology, 2001,
              (C) 2015 KONISHI Yohsuke
License     : BSD-style (see the file LICENSE)
Maintainer  : ocean0yohsuke@gmail.com
Stability   : experimental
Portability : ---

This module enables you to program in Monad-Transformer style for more __deeper__ level than the usual @Control.Monad.Trans@ module expresses.
You would realize exactly what __/more deeper level/__ means by reading the example codes, which are attached on the page bottom.
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module DeepControl.Monad.Trans (
    -- * MonadTrans
    -- ** Level-1
    -- *** trans-lift
    MonadTrans(..), 
    liftT, liftTT, liftTTT, liftTTTT, liftTTTTT,
    -- *** trans-down
    MonadTransDown(..), M,
    -- *** trans-cover  
    MonadTransCover(..),
    -- *** other  
    MonadTrans_(..), 

    -- ** Level-2
    -- *** trans-lift
    MonadTrans2(..), 
    liftTT2, liftTTT2, liftTTTT2, liftTTTTT2,
    -- *** trans-down
    MonadTrans2Down(..), M_, T_, 
    -- *** trans-fold 
    MonadTransFold2(..), 
    -- *** trans-cover  
    MonadTransCover2(..),
    (|**|),
    -- *** other  
    trans2, untrans2, 

    -- ** Level-3
    -- *** trans-lift
    MonadTrans3(..), 
    liftTT3, liftTTT3, liftTTTT3, liftTTTTT3,
    -- *** trans-down
    MonadTrans3Down(..), M__, T__, 
    -- *** trans-fold 
    MonadTransFold3(..), 
    -- *** trans-cover 
    MonadTransCover3(..),
    (|***|), 
    (|-**|), (|*-*|), (|**-|),
    -- *** other  
    trans3, untrans3, 

    -- ** Level-4
    -- *** trans-lift
    MonadTrans4(..),
    -- *** trans-down
    MonadTrans4Down(..), M___, T___, T2__,
    -- *** trans-fold 
    MonadTransFold4(..), 
    -- *** trans-cover 
    MonadTransCover4(..),
    (|****|), 
    (|--**|), (|-*-*|), (|-**-|), (|*-*-|), (|**--|), (|*--*|),
    (|-***|), (|*-**|), (|**-*|), (|***-|), 
    -- *** other  
    trans4, untrans4, 

    -- ** Level-5
    -- *** trans-lift
    MonadTrans5(..),
    -- *** trans-down
    MonadTrans5Down(..), M____, T____, T2___, T3___,
    -- *** trans-fold 
    MonadTransFold5(..), 
    -- *** trans-cover 
    MonadTransCover5(..),
    (|---**|), (|--*-*|), (|-*--*|), (|*---*|), (|*--*-|), (|*-*--|), (|**---|),
    (|--***|), (|-*-**|), (|*--**|), (|*-*-*|), (|**--*|), (|**-*-|), (|***--|),
    (|-****|), (|*-***|), (|**-**|), (|***-*|), (|****-|),
    -- *** other  
    trans5, untrans5, 

    -- * MonadIO
    MonadIO(..),

    -- * Level-2 example
    -- $Example_Level2

    -- * Level-2 example2
    -- $Example_Level2_cover

) where

import DeepControl.Applicative
import DeepControl.Monad

import Control.Monad.IO.Class
import Control.Monad.Trans.Class (MonadTrans (..))
import qualified Control.Monad.List          as L
import qualified Control.Monad.Trans.Maybe   as M
import qualified Control.Monad.Except        as E
import Control.Monad.Identity
import Control.Monad.RWS (RWS, RWST(..), runRWS)
import Control.Monad.Reader (Reader, ReaderT(..), runReader)
import Control.Monad.State (State, StateT(..), runState)

----------------------------------------------------------------------
-- Level-1

-- | Alias to @'lift'@
liftT :: (Monad m, MonadTrans t) => m a -> t m a 
liftT = lift

liftTT
  :: (Monad m, Monad (t1 m), 
      MonadTrans t, MonadTrans t1) =>
     m a -> t (t1 m) a
liftTT = liftT.liftT

liftTTT
  :: (Monad m, Monad (t1 (t2 m)), Monad (t2 m), MonadTrans t,
      MonadTrans t1, MonadTrans t2) =>
     m a -> t (t1 (t2 m)) a
liftTTT = liftT.liftT.liftT

liftTTTT
  :: (Monad m, Monad (t1 (t2 (t3 m))), Monad (t2 (t3 m)), Monad (t3 m), 
      MonadTrans t, MonadTrans t1, MonadTrans t2, MonadTrans t3) =>
     m a -> t (t1 (t2 (t3 m))) a
liftTTTT = liftT.liftT.liftT.liftT

liftTTTTT
  :: (Monad m, Monad (t1 (t2 (t3 (t4 m)))), Monad (t2 (t3 (t4 m))), Monad (t3 (t4 m)), Monad (t4 m), 
      MonadTrans t, MonadTrans t1, MonadTrans t2, MonadTrans t3, MonadTrans t4) =>
     m a -> t (t1 (t2 (t3 (t4 m)))) a
liftTTTTT = liftT.liftT.liftT.liftT.liftT

class (Monad (TransDown t1), MonadTrans t1) => MonadTransDown t1 where
    type TransDown t1 :: * -> *
type M t1 = TransDown t1

instance MonadTransDown L.ListT where
    type TransDown L.ListT = []
instance MonadTransDown M.MaybeT where
    type TransDown M.MaybeT = Maybe
instance MonadTransDown (E.ExceptT e) where
    type TransDown (E.ExceptT e) = E.Except e
instance (Monoid w) => MonadTransDown (RWST r w s) where
    type TransDown (RWST r w s) = RWS r w s
instance MonadTransDown (ReaderT r) where
    type TransDown (ReaderT r) = Reader r
instance MonadTransDown (StateT s) where
    type TransDown (StateT s) = State s

infixl 3  |*|
class (MonadTransDown t1) => MonadTransCover t1 where
    (|*|) :: Monad m1 => (TransDown t1) a -> t1 m1 a

instance MonadTransCover L.ListT where
    (|*|) = L.ListT . (*:)
instance MonadTransCover M.MaybeT where
    (|*|) = M.MaybeT . (*:)
instance MonadTransCover (E.ExceptT e) where
    (|*|) = E.ExceptT . (*:) . E.runExcept
instance (Monoid w) => MonadTransCover (RWST r w s) where
    (|*|) = RWST . ((*:)|$>>) . runRWS
instance MonadTransCover (ReaderT s) where
    (|*|) = ReaderT . ((*:)|$>) . runReader
instance MonadTransCover (StateT s) where
    (|*|) = StateT . ((*:)|$>) . runState

-- | Required only for @'MonadTransFold2'@ and @'MonadTransFold3'@ 
class MonadTrans_ t where
    trans :: (Monad m) => m ((TransDown t) a) -> t m a
    untrans :: (Monad m) => t m a -> m ((TransDown t) a)

instance MonadTrans_ L.ListT where
    trans   = L.ListT
    untrans = L.runListT
instance MonadTrans_ M.MaybeT where
    trans   = M.MaybeT
    untrans = M.runMaybeT
instance MonadTrans_ (E.ExceptT e) where
    untrans x = (E.ExceptT . Identity) |$> E.runExceptT x
    trans x   = E.ExceptT ((runIdentity . E.runExceptT) |$> x)

----------------------------------------------------------------------
-- Level-2

class MonadTrans2 t where
    liftT2 :: (Monad m1, Monad2 m2) => m1 (m2 a) -> t m1 m2 a

liftTT2
  :: (Monad m1, Monad (t2 m1 m2), Monad2 m2,
      MonadTrans t1, MonadTrans2 t2) =>
     m1 (m2 a) -> t1 (t2 m1 m2) a
liftTT2 = liftT.liftT2

liftTTT2
  :: (Monad m1, Monad (t1 (t2 m1 m2)), Monad (t2 m1 m2), 
      Monad2 m2, 
      MonadTrans t, MonadTrans t1, MonadTrans2 t2) =>
     m1 (m2 a) -> t (t1 (t2 m1 m2)) a
liftTTT2 = liftT.liftT.liftT2

liftTTTT2
  :: (Monad m1, Monad (t1 (t1' (t2 m1 m2))), Monad (t1' (t2 m1 m2)), Monad (t2 m1 m2), 
      Monad2 m2, 
      MonadTrans t, MonadTrans t1, MonadTrans t1', MonadTrans2 t2) =>
     m1 (m2 a) -> t (t1 (t1' (t2 m1 m2))) a
liftTTTT2 = liftT.liftT.liftT.liftT2

liftTTTTT2
  :: (Monad m1, Monad (t1 (t1' (t1'' (t2 m1 m2)))), Monad (t1' (t1'' (t2 m1 m2))), Monad (t1'' (t2 m1 m2)), Monad (t2 m1 m2), 
      Monad2 m2, 
      MonadTrans t, MonadTrans t1, MonadTrans t1', MonadTrans t1'', MonadTrans2 t2) =>
     m1 (m2 a) -> t (t1 (t1' (t1'' (t2 m1 m2)))) a
liftTTTTT2 = liftT.liftT.liftT.liftT.liftT2

class (MonadTrans (Trans2Down t2), MonadTrans2 t2) => MonadTrans2Down t2 where
    type Trans2Down t2 :: (* -> *) -> * -> *
type T_ t2 = Trans2Down t2
--type M_ t2 = TransDown (Trans2Down t2)
type M_ t2 = TransDown (T_ t2)

-- | 
--
-- Following property holds.
--
-- > untransfold2 . transfold2 == id
class (MonadTrans (T_ t), MonadTrans2 t) => MonadTransFold2 t where
    transfold2 :: (Monad m1, Monad (t2 m1), 
                   MonadTrans_ t2) => 
                  t m1 (TransDown t2) a -> (T_ t) (t2 m1) a
    untransfold2 :: (Monad m1, Monad (t2 m1), 
                     MonadTrans_ t2) => 
                    (T_ t) (t2 m1) a -> t m1 (TransDown t2) a

infixl 3  |-*|, |*-|, |**|

class (MonadTransCover (Trans2Down t2)) => MonadTransCover2 t2 where
    (|-*|) :: (Monad m1, Monad2 m2) => (Trans2Down t2) m1 a -> t2 m1 m2 a
    (|*-|) :: (Monad m1, Monad2 m2) => (Trans2Down t2) m2 a -> t2 m1 m2 a

(|**|) :: (Monad m1, Monad2 m2, MonadTransCover2 t2) => 
          (M_ t2) a -> t2 m1 m2 a
(|**|) = (|*-|) . (|*|) 

trans2 :: (Monad m, Monad (t1 m), 
           MonadTrans_ t1, MonadTrans_ t2) =>
          m ((TransDown t1) ((TransDown t2) a)) -> t2 (t1 m) a
trans2 = trans . trans
untrans2 :: (Monad m, Monad (t1 m), 
             MonadTrans_ t1, MonadTrans_ t2) =>
            t2 (t1 m) a -> m ((TransDown t1) ((TransDown t2) a))
untrans2 = untrans . untrans

----------------------------------------------------------------------
-- Level-3

class MonadTrans3 t where
    liftT3 :: (Monad m1, Monad2 m2, Monad3 m3) => m1 (m2 (m3 a)) -> t m1 m2 m3 a

liftTT3
  :: (Monad m1, Monad (t3 m1 m2 m3), Monad2 m2, Monad3 m3,
      MonadTrans t, MonadTrans3 t3) =>
     m1 (m2 (m3 a)) -> t (t3 m1 m2 m3) a
liftTT3 = liftT.liftT3

liftTTT3
  :: (Monad m1, Monad (t1 (t3 m1 m2 m3)), Monad (t3 m1 m2 m3), Monad2 m2, Monad3 m3, 
      MonadTrans t, MonadTrans t1, MonadTrans3 t3) =>
     m1 (m2 (m3 a)) -> t (t1 (t3 m1 m2 m3)) a
liftTTT3 = liftT.liftT.liftT3

liftTTTT3
  :: (Monad m1, Monad (t1 (t1' (t3 m1 m2 m3))), Monad (t1' (t3 m1 m2 m3)), Monad (t3 m1 m2 m3),
      Monad2 m2, Monad3 m3, 
      MonadTrans t, MonadTrans t1, MonadTrans t1', MonadTrans3 t3) =>
     m1 (m2 (m3 a)) -> t (t1 (t1' (t3 m1 m2 m3))) a
liftTTTT3 = liftT.liftT.liftT.liftT3

liftTTTTT3
  :: (Monad m1, Monad (t1 (t1' (t1'' (t3 m1 m2 m3)))), Monad (t1' (t1'' (t3 m1 m2 m3))), Monad (t1'' (t3 m1 m2 m3)), Monad (t3 m1 m2 m3), 
      Monad2 m2, Monad3 m3,
      MonadTrans t, MonadTrans t1, MonadTrans t1', MonadTrans t1'', MonadTrans3 t3) =>
     m1 (m2 (m3 a)) -> t (t1 (t1' (t1'' (t3 m1 m2 m3)))) a
liftTTTTT3 = liftT.liftT.liftT.liftT.liftT3

class (MonadTrans2 (Trans3Down t3), MonadTrans3 t3) => MonadTrans3Down t3 where
    type Trans3Down t3 :: (* -> *) -> (* -> *) -> * -> *
type M__ t3 = M_ (Trans3Down t3)
type T__ t3 = T_ (Trans3Down t3)
type T2_ t3 = Trans3Down t3

-- | 
--
-- Following property holds.
--
-- > untransfold3 . transfold3 == id
class (MonadTrans (T__ t), MonadTrans3 t) => MonadTransFold3 t where
    transfold3 :: (Monad m1, Monad (t2 m1), Monad (t3 (t2 m1)), 
                   MonadTrans t2, MonadTrans_ t2, 
                   MonadTrans t3, MonadTrans_ t3) => 
                  t m1 (TransDown t2) (TransDown t3) a -> (T__ t) (t3 (t2 m1)) a
    untransfold3 :: (Monad m1, Monad (t2 m1), Monad (t3 (t2 m1)), 
                     MonadTrans t2, MonadTrans_ t2, 
                     MonadTrans t3, MonadTrans_ t3) => 
                    (T__ t) (t3 (t2 m1)) a -> t m1 (TransDown t2) (TransDown t3) a

infixl 3  |--*|, |-*-|, |*--|
class (MonadTransCover2 (Trans3Down t3)) => MonadTransCover3 t3 where
    (|--*|) :: (Monad m1, Monad2 m2, Monad3 m3) => (Trans3Down t3) m1 m2 a -> t3 m1 m2 m3 a
    (|-*-|) :: (Monad m1, Monad2 m2, Monad3 m3) => (Trans3Down t3) m1 m3 a -> t3 m1 m2 m3 a
    (|*--|) :: (Monad m1, Monad2 m2, Monad3 m3) => (Trans3Down t3) m2 m3 a -> t3 m1 m2 m3 a

infixl 3  |***|
(|***|) :: (Monad m1, Monad2 m2, Monad3 m3, MonadTransCover3 t3) => 
           (M__ t3) a -> t3 m1 m2 m3 a
infixl 3  |-**|, |*-*|, |**-|
(|***|) = (|--*|) . (|**|)
(|-**|) :: (Monad m1, Monad2 m2, Monad3 m3, MonadTransCover3 t3) => 
           (T__ t3) m1 a -> t3 m1 m2 m3 a
(|-**|) = (|--*|) . (|-*|)
(|*-*|) :: (Monad m1, Monad2 m2, Monad3 m3, MonadTransCover3 t3) => 
           (T__ t3) m2 a -> t3 m1 m2 m3 a
(|*-*|) = (|--*|) . (|*-|)
(|**-|) :: (Monad m1, Monad2 m2, Monad3 m3, MonadTransCover3 t3) => 
           (T__ t3) m3 a -> t3 m1 m2 m3 a
(|**-|) = (|-*-|) . (|*-|)

trans3 :: (Monad m, Monad (t2 (t1 m)), Monad (t1 m),
           MonadTrans_ t1, MonadTrans_ t2, MonadTrans_ t3) =>
          m ((TransDown t1) ((TransDown t2) ((TransDown t3) a))) -> t3 (t2 (t1 m)) a
trans3 = trans2 . trans
untrans3 :: (Monad m, Monad (t2 (t1 m)), Monad (t1 m),
             MonadTrans_ t1, MonadTrans_ t2, MonadTrans_ t3) =>
            t3 (t2 (t1 m)) a -> m ((TransDown t1) ((TransDown t2) ((TransDown t3) a)))
untrans3 = untrans2 . untrans

----------------------------------------------------------------------
-- Level-4

class  MonadTrans4 t  where
    liftT4 :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m1 (m2 (m3 (m4 a))) -> t m1 m2 m3 m4 a

class (MonadTrans3 (Trans4Down t4), MonadTrans4 t4) => MonadTrans4Down t4 where
    type Trans4Down t4 :: (* -> *) -> (* -> *) -> (* -> *) -> * -> *
type M___ t4 = M__ (Trans4Down t4)
type T___ t4 = T__ (Trans4Down t4)
type T2__ t4 = Trans3Down (Trans4Down t4)

-- | 
--
-- Following property holds.
--
-- > untransfold4 . transfold4 == id
class (MonadTrans (T___ t), MonadTrans4 t) => MonadTransFold4 t where
    transfold4 :: (Monad m1, Monad (t2 m1), Monad (t3 (t2 m1)), Monad (t4 (t3 (t2 m1))), 
                   MonadTrans t2, MonadTrans_ t2, 
                   MonadTrans t3, MonadTrans_ t3,
                   MonadTrans t4, MonadTrans_ t4) => 
                  t m1 (TransDown t2) (TransDown t3) (TransDown t4) a -> (T___ t) (t4 (t3 (t2 m1))) a
    untransfold4 :: (Monad m1, Monad (t2 m1), Monad (t3 (t2 m1)), Monad (t4 (t3 (t2 m1))), 
                     MonadTrans t2, MonadTrans_ t2, 
                     MonadTrans t3, MonadTrans_ t3,
                     MonadTrans t4, MonadTrans_ t4) => 
                    (T___ t) (t4 (t3 (t2 m1))) a -> t m1 (TransDown t2) (TransDown t3) (TransDown t4) a

infixl 3  |---*|, |--*-|, |-*--|, |*---|
class (MonadTransCover3 (Trans4Down t4)) => MonadTransCover4 t4 where
    (|---*|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => (Trans4Down t4) m1 m2 m3 a -> t4 m1 m2 m3 m4 a
    (|--*-|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => (Trans4Down t4) m1 m2 m4 a -> t4 m1 m2 m3 m4 a
    (|-*--|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => (Trans4Down t4) m1 m3 m4 a -> t4 m1 m2 m3 m4 a
    (|*---|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => (Trans4Down t4) m2 m3 m4 a -> t4 m1 m2 m3 m4 a

infixl 3  |****|
(|****|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, MonadTransCover4 t4) => 
            (M___ t4) a -> t4 m1 m2 m3 m4 a
(|****|) = (|---*|) . (|***|)
infixl 3  |--**|, |-*-*|, |-**-|, |*-*-|, |**--|, |*--*|
(|--**|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, MonadTransCover4 t4) => 
            (T2__ t4) m1 m2 a -> t4 m1 m2 m3 m4 a
(|--**|) = (|---*|) . (|--*|)
(|-*-*|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, MonadTransCover4 t4) => 
            (T2__ t4) m1 m3 a -> t4 m1 m2 m3 m4 a
(|-*-*|) = (|---*|) . (|-*-|)
(|-**-|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, MonadTransCover4 t4) => 
            (T2__ t4) m1 m4 a -> t4 m1 m2 m3 m4 a
(|-**-|) = (|--*-|) . (|-*-|)
(|*-*-|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, MonadTransCover4 t4) => 
            (T2__ t4) m2 m4 a -> t4 m1 m2 m3 m4 a
(|*-*-|) = (|--*-|) . (|*--|)
(|**--|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, MonadTransCover4 t4) => 
            (T2__ t4) m3 m4 a -> t4 m1 m2 m3 m4 a
(|**--|) = (|-*--|) . (|*--|)
(|*--*|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, MonadTransCover4 t4) => 
            (T2__ t4) m2 m3 a -> t4 m1 m2 m3 m4 a
(|*--*|) = (|---*|) . (|*--|)
infixl 3  |-***|, |*-**|, |**-*|, |***-| 
(|-***|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, MonadTransCover4 t4) => 
            (T___ t4) m1 a -> t4 m1 m2 m3 m4 a
(|-***|) = (|---*|) . (|-**|)
(|*-**|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, MonadTransCover4 t4) => 
            (T___ t4) m2 a -> t4 m1 m2 m3 m4 a
(|*-**|) = (|---*|) . (|*-*|)
(|**-*|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, MonadTransCover4 t4) => 
            (T___ t4) m3 a -> t4 m1 m2 m3 m4 a
(|**-*|) = (|---*|) . (|**-|)
(|***-|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, MonadTransCover4 t4) => 
            (T___ t4) m4 a -> t4 m1 m2 m3 m4 a
(|***-|) = (|--*-|) . (|**-|)

trans4 :: (Monad m, Monad (t3 (t2 (t1 m))), Monad (t2 (t1 m)), Monad (t1 m),
           MonadTrans_ t1, MonadTrans_ t2, MonadTrans_ t3, MonadTrans_ t4) =>
          m ((TransDown t1) ((TransDown t2) ((TransDown t3) ((TransDown t4) a)))) -> t4 (t3 (t2 (t1 m))) a
trans4 = trans3 . trans
untrans4 :: (Monad m, Monad (t3 (t2 (t1 m))), Monad (t2 (t1 m)), Monad (t1 m),
             MonadTrans_ t1, MonadTrans_ t2, MonadTrans_ t3, MonadTrans_ t4) =>
            t4 (t3 (t2 (t1 m))) a -> m ((TransDown t1) ((TransDown t2) ((TransDown t3) ((TransDown t4) a))))
untrans4 = untrans3 . untrans

----------------------------------------------------------------------
-- Level-5

class MonadTrans5 t where
    liftT5 :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m1 (m2 (m3 (m4 (m5 a)))) -> t m1 m2 m3 m4 m5 a

class (MonadTrans4 (Trans5Down t5), MonadTrans5 t5) => MonadTrans5Down t5 where
    type Trans5Down t5 :: (* -> *) -> (* -> *) -> (* -> *) -> (* -> *) -> * -> *
type M____ t5 = M___ (Trans5Down t5)
type T____ t5 = T___ (Trans5Down t5)
type T2___ t5 = T2__ (Trans5Down t5)
type T3___ t5 = Trans4Down (Trans5Down t5)

-- | 
--
-- Following property holds.
--
-- > untransfold5 . transfold5 == id
class (MonadTrans (T____ t), MonadTrans5 t) => MonadTransFold5 t where
    transfold5 :: (Monad m1, Monad (t2 m1), Monad (t3 (t2 m1)), Monad (t4 (t3 (t2 m1))), Monad (t5 (t4 (t3 (t2 m1)))), 
                   MonadTrans t2, MonadTrans_ t2, 
                   MonadTrans t3, MonadTrans_ t3,
                   MonadTrans t4, MonadTrans_ t4,
                   MonadTrans t5, MonadTrans_ t5) => 
                  t m1 (TransDown t2) (TransDown t3) (TransDown t4) (TransDown t5) a -> (T____ t) (t5 (t4 (t3 (t2 m1)))) a
    untransfold5 :: (Monad m1, Monad (t2 m1), Monad (t3 (t2 m1)), Monad (t4 (t3 (t2 m1))), Monad (t5 (t4 (t3 (t2 m1)))), 
                     MonadTrans t2, MonadTrans_ t2, 
                     MonadTrans t3, MonadTrans_ t3,
                     MonadTrans t4, MonadTrans_ t4,
                     MonadTrans t5, MonadTrans_ t5) => 
                    (T____ t) (t5 (t4 (t3 (t2 m1)))) a -> t m1 (TransDown t2) (TransDown t3) (TransDown t4) (TransDown t5) a

infixl 3  |----*|, |---*-|, |--*--|, |-*---|, |*----|
class (MonadTransCover4 (Trans5Down t5)) => MonadTransCover5 t5 where
    (|----*|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => (Trans5Down t5) m1 m2 m3 m4 a -> t5 m1 m2 m3 m4 m5 a
    (|---*-|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => (Trans5Down t5) m1 m2 m3 m5 a -> t5 m1 m2 m3 m4 m5 a
    (|--*--|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => (Trans5Down t5) m1 m2 m4 m5 a -> t5 m1 m2 m3 m4 m5 a
    (|-*---|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => (Trans5Down t5) m1 m3 m4 m5 a -> t5 m1 m2 m3 m4 m5 a
    (|*----|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => (Trans5Down t5) m2 m3 m4 m5 a -> t5 m1 m2 m3 m4 m5 a

infixl 3  |*****|
(|*****|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5, MonadTransCover5 t5) => 
            (M____ t5) a -> t5 m1 m2 m3 m4 m5 a
(|*****|) = (|----*|) . (|****|)
infixl 3  |---**|, |--*-*|, |-*--*|, |*---*|, |*--*-|, |*-*--|, |**---|
(|---**|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5, MonadTransCover5 t5) => 
            (T3___ t5) m1 m2 m3 a -> t5 m1 m2 m3 m4 m5 a
(|---**|) = (|----*|) . (|---*|)
(|--*-*|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5, MonadTransCover5 t5) => 
            (T3___ t5) m1 m2 m4 a -> t5 m1 m2 m3 m4 m5 a
(|--*-*|) = (|----*|) . (|--*-|)
(|-*--*|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5, MonadTransCover5 t5) => 
            (T3___ t5) m1 m3 m4 a -> t5 m1 m2 m3 m4 m5 a
(|-*--*|) = (|----*|) . (|-*--|)
(|*---*|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5, MonadTransCover5 t5) => 
            (T3___ t5) m2 m3 m4 a -> t5 m1 m2 m3 m4 m5 a
(|*---*|) = (|----*|) . (|*---|)
(|*--*-|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5, MonadTransCover5 t5) => 
            (T3___ t5) m2 m3 m5 a -> t5 m1 m2 m3 m4 m5 a
(|*--*-|) = (|---*-|) . (|*---|)
(|*-*--|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5, MonadTransCover5 t5) => 
            (T3___ t5) m2 m4 m5 a -> t5 m1 m2 m3 m4 m5 a
(|*-*--|) = (|--*--|) . (|*---|)
(|**---|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5, MonadTransCover5 t5) => 
            (T3___ t5) m3 m4 m5 a -> t5 m1 m2 m3 m4 m5 a
(|**---|) = (|-*---|) . (|*---|)
infixl 3  |--***|, |-*-**|, |*--**|, |*-*-*|, |**--*|, |**-*-|, |***--|
(|--***|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5, MonadTransCover5 t5) => 
            (T2___ t5) m1 m2 a -> t5 m1 m2 m3 m4 m5 a
(|--***|) = (|----*|) . (|--**|)
(|-*-**|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5, MonadTransCover5 t5) => 
            (T2___ t5) m1 m3 a -> t5 m1 m2 m3 m4 m5 a
(|-*-**|) = (|----*|) . (|-*-*|)
(|*--**|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5, MonadTransCover5 t5) => 
            (T2___ t5) m2 m3 a -> t5 m1 m2 m3 m4 m5 a
(|*--**|) = (|----*|) . (|*--*|)
(|*-*-*|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5, MonadTransCover5 t5) => 
            (T2___ t5) m2 m4 a -> t5 m1 m2 m3 m4 m5 a
(|*-*-*|) = (|----*|) . (|*-*-|)
(|**--*|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5, MonadTransCover5 t5) => 
            (T2___ t5) m3 m4 a -> t5 m1 m2 m3 m4 m5 a
(|**--*|) = (|----*|) . (|**--|)
(|**-*-|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5, MonadTransCover5 t5) => 
            (T2___ t5) m3 m5 a -> t5 m1 m2 m3 m4 m5 a
(|**-*-|) = (|---*-|) . (|**--|)
(|***--|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5, MonadTransCover5 t5) => 
            (T2___ t5) m4 m5 a -> t5 m1 m2 m3 m4 m5 a
(|***--|) = (|--*--|) . (|**--|)
infixl 3  |-****|, |*-***|, |**-**|, |***-*|, |****-|
(|-****|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5, MonadTransCover5 t5) => 
            (T____ t5) m1 a -> t5 m1 m2 m3 m4 m5 a
(|-****|) = (|----*|) . (|-***|)
(|*-***|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5, MonadTransCover5 t5) => 
            (T____ t5) m2 a -> t5 m1 m2 m3 m4 m5 a
(|*-***|) = (|----*|) . (|*-**|)
(|**-**|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5, MonadTransCover5 t5) => 
            (T____ t5) m3 a -> t5 m1 m2 m3 m4 m5 a
(|**-**|) = (|----*|) . (|**-*|)
(|***-*|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5, MonadTransCover5 t5) => 
            (T____ t5) m4 a -> t5 m1 m2 m3 m4 m5 a
(|***-*|) = (|----*|) . (|***-|)
(|****-|) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5, MonadTransCover5 t5) => 
            (T____ t5) m5 a -> t5 m1 m2 m3 m4 m5 a
(|****-|) = (|---*-|) . (|***-|)

trans5 :: (Monad m, Monad (t4 (t3 (t2 (t1 m)))), Monad (t3 (t2 (t1 m))), Monad (t2 (t1 m)), Monad (t1 m),
           MonadTrans_ t1, MonadTrans_ t2, MonadTrans_ t3, MonadTrans_ t4, MonadTrans_ t5) =>
          m ((TransDown t1) ((TransDown t2) ((TransDown t3) ((TransDown t4) ((TransDown t5) a))))) -> t5 (t4 (t3 (t2 (t1 m)))) a
trans5 = trans4 . trans
untrans5 :: (Monad m, Monad (t4 (t3 (t2 (t1 m)))), Monad (t3 (t2 (t1 m))), Monad (t2 (t1 m)), Monad (t1 m),
             MonadTrans_ t1, MonadTrans_ t2, MonadTrans_ t3, MonadTrans_ t4, MonadTrans_ t5) =>
            t5 (t4 (t3 (t2 (t1 m)))) a -> m ((TransDown t1) ((TransDown t2) ((TransDown t3) ((TransDown t4) ((TransDown t5) a)))))
untrans5 = untrans4 . untrans


----------------------------------------------------------------------
-- Examples

{- $Example_Level2
Here is a monad transformer example how to implement Ackermann function, improved to stop within a certain limit of time, with ReaderT-IdentityT2-IO-Maybe monad, a level-2 monad-transformation.

>import DeepControl.Applicative
>import DeepControl.Commutative (commute)
>import DeepControl.Monad ((>-))
>import DeepControl.Monad.Morph ((|>|))
>import DeepControl.Monad.Trans (liftTT2, transfold2, untransfold2)
>import DeepControl.Monad.Trans.Identity
>import Control.Monad.Reader
>import Control.Monad.Trans.Maybe
>
>import System.Timeout (timeout)
>
>type TimeLimit = Int
>
>ackermannTimeLimit :: TimeLimit -> Int -> Int -> 
>                      IO (Maybe Int)                     -- IO-Maybe Monad
>ackermannTimeLimit timelimit x y = timeout timelimit (ackermannIO x y)
>  where
>    ackermannIO :: Int -> Int -> IO Int
>    ackermannIO 0 n = (*:) $ n + 1
>    ackermannIO m n | m > 0 && n == 0 = ackermannIO (m-1) 1
>                    | m > 0 && n > 0  = ackermannIO m (n-1) >>= ackermannIO (m-1)
> 
>ackermann :: Int -> Int -> 
>             ReaderT TimeLimit (IdentityT2 IO Maybe) Int -- ReaderT-IdentityT2-IO-Maybe monad
>ackermann x y = do
>    timelimit <- ask
>    liftTT2 $ ackermannTimeLimit timelimit x y           -- lift IO-Maybe function to ReaderT-IdentityT2-IO-Maybe function
>
>calc_ackermann :: TimeLimit -> Int -> Int -> IO (Maybe Int)
>calc_ackermann timelimit x y = ackermann x y >- \r -> runReaderT r timelimit
>                                             >- runIdentityT2
>
>-- λ> commute $ calc_ackermann 1000 |$> [0..4] |* 4
>-- [Just 5,Just 6,Just 11,Just 125,Nothing]
>
>ackermann' :: Int -> Int -> 
>              ReaderT TimeLimit (MaybeT IO) Int                -- ReaderT-MaybeT-IO monad
>ackermann' x y = (runIdentityT . transfold2) |>| ackermann x y -- You can get usual ReaderT-MaybeT-IO function from ReaderT-IdentityT2-IO-Maybe function
>
>ackermann'' :: Int -> Int -> 
>               ReaderT TimeLimit (IdentityT2 IO Maybe) Int      -- ReaderT-IdentityT2-IO-Maybe monad
>ackermann'' x y = (untransfold2 . IdentityT) |>| ackermann' x y -- You can get ReaderT-IdentityT2-IO-Maybe function from usual ReaderT-MaybeT-IO function
-}

{- $Example_Level2_cover
Here is a monad transformer example showing how to use trans-cover functions.

>import DeepControl.Applicative ((|$>))
>import DeepControl.Monad (Monad2)
>import DeepControl.Monad.Morph ((|>|))
>import DeepControl.Monad.Trans (liftT, (|*|), (|-*|), (|*-|))
>import DeepControl.Monad.Trans.Writer
>import DeepControl.Monad.Trans.Identity
>import Control.Monad.State
>
>tick :: State Int ()
>tick = modify (+1)
>
>tock                         ::                   StateT Int IO ()
>tock = do
>    (|*|) tick               :: (Monad      m) => StateT Int m  ()  -- (|*|) is the level-1 trans-cover function, analogous for (*:)
>    liftT $ putStrLn "Tock!" :: (MonadTrans t) => t          IO ()  -- 'liftT' is the level-1 trans-lift function, alias to 'lift'
>
>-- λ> runStateT tock 0
>-- Tock!
>-- ((),1)
>
>save :: StateT Int (Writer [Int]) ()
>save = do
>    n <- get
>    liftT $ tell [n]
>
>program ::               StateT Int (IdentityT2 IO (Writer [Int])) ()  -- StateT-IdentityT2-IO-Writer monad, a level-2 monad-transform
>program = replicateM_ 4 $ do
>    ((|-*|).liftT) |>| tock                                            -- (|-*|) is a level-2 trans-cover function, analogous for (-*)
>        :: (Monad2 m) => StateT Int (IdentityT2 IO m             ) ()
>    ((|*-|).liftT) |>| save                                            -- (|*-|) is a level-2 trans-cover function, analogous for (*-)
>        :: (Monad  m) => StateT Int (IdentityT2 m  (Writer [Int])) ()
>
>-- λ> execWriter |$> runIdentityT2 (runStateT program 0)
>-- Tock!
>-- Tock!
>-- Tock!
>-- Tock!
>-- [1,2,3,4]
-}
