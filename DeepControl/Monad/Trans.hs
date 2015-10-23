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
Note: many instances for Level-4 and Level-5 haven't been written yet.
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module DeepControl.Monad.Trans (
    -- * MonadIO
    MonadIO(..),

    -- * MonadTrans
    -- ** Level-1
    MonadTrans(..), 
    MonadTransDown(..), M,
    -- *** cover  
    MonadTransCover(..),
    -- *** other  
    MonadTrans_(..), 

    -- ** Level-2
    MonadTrans2(..), 
    MonadTrans2Down(..), M_, T_, 
    -- *** fold 
    MonadTransFold2(..), 
    -- *** cover  
    MonadTransCover2(..),
    (|**|),
    -- *** other  
    trans2, untrans2, 

    -- ** Level-3
    MonadTrans3(..), 
    MonadTrans3Down(..), M__, T__, 
    -- *** fold 
    MonadTransFold3(..), 
    -- *** cover 
    MonadTransCover3(..),
    (|***|), (|-**|), (|*-*|), (|**-|),
    -- *** other  
    trans3, untrans3, 

    -- ** Level-4
    MonadTrans4(..),

    -- ** Level-5
    MonadTrans5(..),

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

----------------------------------------------------------------------
-- Level-1

class (Monad (TransDown t1), MonadTrans t1) => MonadTransDown t1 where
    type TransDown t1 :: * -> *
type M t1 = TransDown t1

instance MonadTransDown L.ListT where
    type TransDown L.ListT = []
instance MonadTransDown M.MaybeT where
    type TransDown M.MaybeT = Maybe
instance MonadTransDown (E.ExceptT e) where
    type TransDown (E.ExceptT e) = E.Except e

{-
class (MonadTrans t) => MonadTransFold t where
    transfold :: (Monad m1) => 
                  m1 a -> t m1 a
-}

infixl 3  |*|

class (MonadTransDown t1) => MonadTransCover t1 where
    (|*|) :: Monad m1 => (TransDown t1) a -> t1 m1 a

instance MonadTransCover L.ListT where
    (|*|) = L.ListT . (*:)
instance MonadTransCover M.MaybeT where
    (|*|) = M.MaybeT . (*:)
instance MonadTransCover (E.ExceptT e) where
    (|*|) = E.ExceptT . (*:) . E.runExcept

-- | Required only for @'MonadTransFold2'@ and @'MonadTransFold3'@ 
class MonadTrans_ t1 where
    trans :: (Monad m2) => m2 ((TransDown t1) a) -> t1 m2 a
    untrans :: (Monad m2) => t1 m2 a -> m2 ((TransDown t1) a)

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
    lift2 :: (Monad m1, Monad2 m2) => m1 (m2 a) -> t m1 m2 a

class (MonadTrans (Trans2Down t2), MonadTrans2 t2) => MonadTrans2Down t2 where
    type Trans2Down t2 :: (* -> *) -> * -> *
type M_ t2 = TransDown (Trans2Down t2)
type T_ t2 = Trans2Down t2


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

trans2 :: (Monad m1, Monad (t2 m1), 
           MonadTrans_ t2, MonadTrans_ t3) =>
           m1 ((TransDown t2) ((TransDown t3) a)) -> t3 (t2 m1) a
trans2 = trans . trans
untrans2 :: (Monad m1, Monad (t2 m1), 
             MonadTrans_ t2, MonadTrans_ t3) =>
             t3 (t2 m1) a -> m1 ((TransDown t2) ((TransDown t3) a))
untrans2 = untrans . untrans

----------------------------------------------------------------------
-- Level-3

class MonadTrans3 t where
    lift3 :: (Monad m1, Monad2 m2, Monad3 m3) => m1 (m2 (m3 a)) -> t m1 m2 m3 a

class (MonadTrans2 (Trans3Down t3), MonadTrans3 t3) => MonadTrans3Down t3 where
    type Trans3Down t3 :: (* -> *) -> (* -> *) -> * -> *
type M__ t3 = M_ (Trans3Down t3)
type T__ t3 = T_ (Trans3Down t3)

-- | 
--
-- Following property holds.
--
-- > untransfold3 . transfold3 == id
class (MonadTrans (T__ t), MonadTrans3 t) => MonadTransFold3 t where
    transfold3 :: (Monad m1, Monad (t2 m1), Monad (t3 (t2 m1)), 
                   MonadTrans t3, MonadTrans t2, 
                   MonadTrans_ t2, MonadTrans_ t3) => 
                  t m1 (TransDown t2) (TransDown t3) a -> (T__ t) (t3 (t2 m1)) a
    untransfold3 :: (Monad m1, Monad (t2 m1), Monad (t3 (t2 m1)), 
                     MonadTrans t3, MonadTrans t2, 
                     MonadTrans_ t2, MonadTrans_ t3) => 
                    (T__ t) (t3 (t2 m1)) a -> t m1 (TransDown t2) (TransDown t3) a

infixl 3  |--*|, |-*-|, |*--|, |***|, |-**|, |*-*|, |**-|

class (MonadTransCover2 (Trans3Down t3)) => MonadTransCover3 t3 where
    (|--*|) :: (Monad m1, Monad2 m2, Monad3 m3) => (Trans3Down t3) m1 m2 a -> t3 m1 m2 m3 a
    (|-*-|) :: (Monad m1, Monad2 m2, Monad3 m3) => (Trans3Down t3) m1 m3 a -> t3 m1 m2 m3 a
    (|*--|) :: (Monad m1, Monad2 m2, Monad3 m3) => (Trans3Down t3) m2 m3 a -> t3 m1 m2 m3 a

(|***|) :: (Monad m1, Monad2 m2, Monad3 m3, MonadTransCover3 t3) => 
           (M__ t3) a -> t3 m1 m2 m3 a
(|***|) = (|*--|) . (|**|)
(|-**|) :: (Monad m1, Monad2 m2, Monad3 m3, MonadTransCover3 t3) => 
           (T__ t3) m1 a -> t3 m1 m2 m3 a
(|-**|) = (|--*|) . (|-*|)
(|*-*|) :: (Monad m1, Monad2 m2, Monad3 m3, MonadTransCover3 t3) => 
           (T__ t3) m2 a -> t3 m1 m2 m3 a
(|*-*|) = (|--*|) . (|*-|)
(|**-|) :: (Monad m1, Monad2 m2, Monad3 m3, MonadTransCover3 t3) => 
           (T__ t3) m3 a -> t3 m1 m2 m3 a
(|**-|) = (|*--|) . (|*-|)

trans3 :: (Monad m1, Monad (t3 (t2 m1)), Monad (t2 m1),
           MonadTrans_ t2, MonadTrans_ t3, MonadTrans_ t4) =>
           m1 ((TransDown t2) ((TransDown t3) ((TransDown t4) a))) -> t4 (t3 (t2 m1)) a
trans3 = trans2 . trans
untrans3 :: (Monad m1, Monad (t3 (t2 m1)), Monad (t2 m1),
           MonadTrans_ t2, MonadTrans_ t3, MonadTrans_ t4) =>
           t4 (t3 (t2 m1)) a -> m1 ((TransDown t2) ((TransDown t3) ((TransDown t4) a)))
untrans3 = untrans2 . untrans


----------------------------------------------------------------------
-- Level-4

class  MonadTrans4 t  where
    lift4 :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m1 (m2 (m3 (m4 a))) -> t m1 m2 m3 m4 a

----------------------------------------------------------------------
-- Level-5

class MonadTrans5 t where
    lift5 :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m1 (m2 (m3 (m4 (m5 a)))) -> t m1 m2 m3 m4 m5 a

----------------------------------------------------------------------
-- Examples

{- $Example_Level2
Here is a monad transformer example how to implement Ackermann function, improved to stop within a certain limit of time, with ReaderT-IdentityT2-IO-Maybe monad, a level-2 monad-transformation.

>import DeepControl.Applicative
>import DeepControl.Commutative (commute)
>import DeepControl.Monad ((>-))
>import DeepControl.Monad.Morph ((|>|))
>import DeepControl.Monad.Trans (lift2, transfold2, untransfold2)
>import DeepControl.Monad.Trans.Reader
>import DeepControl.Monad.Trans.Identity
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
>    lift . lift2 $ ackermannTimeLimit timelimit x y      -- lift IO-Maybe function to ReaderT-IdentityT2-IO-Maybe function
>
>calc_ackermann :: TimeLimit -> Int -> Int -> IO (Maybe Int)
>calc_ackermann timelimit x y = ackermann x y >- \r -> runReaderT r timelimit
>                                             >- runIdentityT2
>
>-- λ> commute $ calc_ackermann 1000 |$> [0..4] |* 4
>-- [Just 5,Just 6,Just 11,Just 125,Nothing]
>
>ackermann' :: Int -> Int -> 
>              ReaderT TimeLimit (IdentityT (MaybeT IO)) Int -- ReaderT-IdentityT-MaybeT-IO monad
>ackermann' x y = transfold2 |>| ackermann x y               -- You can get usual ReaderT-IdentityT-MaybeT-IO function from ReaderT-IdentityT2-IO-Maybe function
>
>ackermann'' :: Int -> Int -> 
>               ReaderT TimeLimit (IdentityT2 IO Maybe) Int -- ReaderT-IdentityT2-IO-Maybe monad
>ackermann'' x y = untransfold2 |>| ackermann' x y          -- You can get ReaderT-IdentityT2-IO-Maybe function from usual ReaderT-Identity-MaybeT-IO function
-}

{- $Example_Level2_cover
Here is a monad transformer example showing how to use cover functions.

>import DeepControl.Applicative ((|$>))
>import DeepControl.Monad (Monad2)
>import DeepControl.Monad.Morph ((|>|))
>import DeepControl.Monad.Trans (lift, (|*|), (|-*|), (|*-|))
>import DeepControl.Monad.Trans.Writer
>import DeepControl.Monad.Trans.Identity
>import DeepControl.Monad.Trans.State
>
>tick :: State Int ()
>tick = modify (+1)
>
>tock                        ::                   StateT Int IO ()
>tock = do
>    (|*|) tick              :: (Monad      m) => StateT Int m  ()
>    lift $ putStrLn "Tock!" :: (MonadTrans t) => t          IO ()
>
>-- λ> runStateT tock 0
>-- Tock!
>-- ((),1)
>
>save    :: StateT Int (Writer [Int]) ()
>save = do
>    n <- get
>    lift $ tell [n]
>
>program ::               StateT Int (IdentityT2 IO (Writer [Int])) ()
>program = replicateM_ 4 $ do
>    ((|-*|).lift) |>| tock
>        :: (Monad2 m) => StateT Int (IdentityT2 IO m             ) ()
>    ((|*-|).lift) |>| save
>        :: (Monad  m) => StateT Int (IdentityT2 m  (Writer [Int])) ()
>
>-- λ> execWriter |$> (runIdentityT2 $ runStateT program 0)
>-- Tock!
>-- Tock!
>-- Tock!
>-- Tock!
>-- [1,2,3,4]
-}
