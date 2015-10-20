{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
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
Note: all the MonadTransx instances for Level-4 and Level-5 haven't been written yet.
-}
module DeepControl.Monad.Trans (
    -- * MonadIO
    MonadIO(..),

    -- * MonadTrans
    -- ** Level-1
    MonadTrans(..), MonadTrans_(..),
    -- ** Level-2
    MonadTrans2(..), trans2, untrans2, MonadTransFold2(..),
    -- ** Level-3
    MonadTrans3(..), trans3, untrans3, MonadTransFold3(..),
    -- ** Level-4
    MonadTrans4(..),
    -- ** Level-5
    MonadTrans5(..),

    -- * Level-2 example
    -- $Example_Level2

) where

import DeepControl.Monad

import Control.Monad.IO.Class
import Control.Monad.Trans.Class (MonadTrans (..))
import qualified Control.Monad.List          as L
import qualified Control.Monad.Trans.Maybe   as M
import qualified Control.Monad.Except        as E

----------------------------------------------------------------------
-- Level-1

-- | Required for @'MonadTransFold2'@ and @'MonadTransFold3'@ 
class (MonadTrans t1) => MonadTrans_ m1 t1 where
    trans :: (Monad m2) => m2 (m1 a) -> t1 m2 a
    untrans :: (Monad m2) => t1 m2 a -> m2 (m1 a)

instance MonadTrans_ [] L.ListT where
    trans   = L.ListT
    untrans = L.runListT
instance MonadTrans_ Maybe M.MaybeT where
    trans   = M.MaybeT
    untrans = M.runMaybeT
instance MonadTrans_ (Either e) (E.ExceptT e) where
    trans   = E.ExceptT
    untrans = E.runExceptT

{-
class (MonadTrans t) => MonadTransFold t where
    transfold :: (Monad m1) => 
                  m1 a -> t m1 a
-}

----------------------------------------------------------------------
-- Level-2

class MonadTrans2 t where
    lift2 :: (Monad m1, Monad2 m2) => m1 (m2 a) -> t m1 m2 a

trans2 :: (Monad m1, Monad (t2 m1), 
           MonadTrans_ m2 t2, MonadTrans_ m3 t3) =>
           m1 (m2 (m3 a)) -> t3 (t2 m1) a
trans2 = trans . trans
untrans2 :: (Monad m1, Monad (t2 m1), 
             MonadTrans_ m2 t2, MonadTrans_ m3 t3) =>
             t3 (t2 m1) a -> m1 (m2 (m3 a))
untrans2 = untrans . untrans

-- | 
--
-- Following property holds.
--
-- > untransfold2 . transfold2 == id
class (MonadTrans t1, MonadTrans2 t) => MonadTransFold2 t1 t where
    transfold2 :: (Monad m1, Monad (t2 m1), 
                   MonadTrans_ m2 t2) => 
                  t m1 m2 a -> t1 (t2 m1) a
    untransfold2 :: (Monad m1, Monad (t2 m1), 
                     MonadTrans_ m2 t2) => 
                    t1 (t2 m1) a -> t m1 m2 a

----------------------------------------------------------------------
-- Level-3

class MonadTrans3 t where
    lift3 :: (Monad m1, Monad2 m2, Monad3 m3) => m1 (m2 (m3 a)) -> t m1 m2 m3 a

trans3 :: (Monad m1, Monad (t3 (t2 m1)), Monad (t2 m1),
           MonadTrans_ m2 t2, MonadTrans_ m3 t3, MonadTrans_ m4 t4) =>
           m1 (m2 (m3 (m4 a))) -> t4 (t3 (t2 m1)) a
trans3 = trans2 . trans
untrans3 :: (Monad m1, Monad (t3 (t2 m1)), Monad (t2 m1),
           MonadTrans_ m2 t2, MonadTrans_ m3 t3, MonadTrans_ m4 t4) =>
           t4 (t3 (t2 m1)) a -> m1 (m2 (m3 (m4 a)))
untrans3 = untrans2 . untrans

-- | 
--
-- Following property holds.
--
-- > untransfold3 . transfold3 == id
class (MonadTrans t1, MonadTrans3 t) => MonadTransFold3 t1 t where
    transfold3 :: (Monad m1, Monad (t2 m1), Monad (t3 (t2 m1)), 
                   MonadTrans t3, MonadTrans t2, 
                   MonadTrans_ m2 t2, MonadTrans_ m3 t3) => 
                  t m1 m2 m3 a -> t1 (t3 (t2 m1)) a
    untransfold3 :: (Monad m1, Monad (t2 m1), Monad (t3 (t2 m1)), 
                     MonadTrans t3, MonadTrans t2, 
                     MonadTrans_ m2 t2, MonadTrans_ m3 t3) => 
                    t1 (t3 (t2 m1)) a -> t m1 m2 m3 a

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
Here is a monad transformer example how to implement Ackermann function, improved to stop within a certain limit of time, with ReaderT2-IO-Maybe monad, a level-2 monad-transformation.

>import DeepControl.Applicative
>import DeepControl.Commutative (commute)
>import DeepControl.Monad ((>-))
>import DeepControl.Monad.Trans (lift2, transfold2, untransfold2)
>import DeepControl.Monad.Trans.Reader
>import Control.Monad.Trans.Maybe
>
>import System.Timeout (timeout)
>
>type TimeLimit = Int
>
>ackermannTimeLimit :: TimeLimit -> Int -> Int
>                      -> IO (Maybe Int)             -- IO-Maybe Monad
>ackermannTimeLimit timelimit x y = timeout timelimit (ackermannIO x y)
>  where
>    ackermannIO :: Int -> Int -> IO Int
>    ackermannIO 0 n = (*:) $ n + 1
>    ackermannIO m n | m > 0 && n == 0 = ackermannIO (m-1) 1
>                    | m > 0 && n > 0  = ackermannIO m (n-1) >>= ackermannIO (m-1)
> 
>ackermann :: Int -> Int -> 
>               ReaderT2 TimeLimit IO Maybe Int      -- ReaderT2-IO-Maybe monad
>ackermann x y = do
>    timelimit <- ask
>    lift2 $ ackermannTimeLimit timelimit x y        -- lift IO-Maybe function to ReaderT2-IO-Maybe function
>
>calc_ackermann :: TimeLimit -> Int -> Int -> IO (Maybe Int)
>calc_ackermann timelimit x y = ackermann x y >- \r -> runReaderT2 r timelimit
>
>-- λ> commute $ calc_ackermann 1000 |$> [0..4] |* 4
>-- [Just 5,Just 6,Just 11,Just 125,Nothing]
>
>ackermann' :: Int -> Int -> 
>              ReaderT TimeLimit (MaybeT IO) Int     -- ReaderT-MaybeT-IO monad
>ackermann' x y = transfold2 $ ackermann x y         -- You can get usual ReaderT-MaybeT-IO function from ReaderT2-IO-Maybe function
>
>calc_ackermann' :: TimeLimit -> Int -> Int -> IO (Maybe Int)
>calc_ackermann' timelimit x y = ackermann' x y >- \r -> runReaderT r timelimit
>                                               >- runMaybeT
>
>-- λ> commute $ calc_ackermann' 1000 |$> [0..4] |* 4
>-- [Just 5,Just 6,Just 11,Just 125,Nothing]
>
>ackermann'' :: Int -> Int -> 
>               ReaderT2 TimeLimit IO Maybe Int       -- ReaderT2-IO-Maybe monad
>ackermann'' x y = untransfold2 $ ackermann' x y      -- You can get ReaderT2-IO-Maybe function from usual ReaderT-MaybeT-IO function
>
>calc_ackermann'' :: TimeLimit -> Int -> Int -> IO (Maybe Int)
>calc_ackermann'' timelimit x y = ackermann'' x y >- \r -> runReaderT2 r timelimit
>
>-- λ> commute $ calc_ackermann'' 1000 |$> [0..4] |* 4
>-- [Just 5,Just 6,Just 11,Just 125,Nothing]
-}
