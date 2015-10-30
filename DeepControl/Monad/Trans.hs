{-|
Module      : DeepControl.Monad.Trans
Description : Deepened the usual Control.Monad.Trans module.
Copyright   : (c) Andy Gill 2001,
              (c) Oregon Graduate Institute of Science and Technology, 2001,
              (c) 2015 KONISHI Yohsuke
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
{-# LANGUAGE FunctionalDependencies #-}
module DeepControl.Monad.Trans (
    module Control.Monad.Trans,
    -- * MonadTrans
    MonadTrans_(..), 

    -- * Level-2
    -- ** trans-roll
    transfold2, untransfold2,

    -- * Level-3
    -- ** trans-roll
    transfold3, untransfold3,

    -- * Level-4
    -- ** trans-roll
    transfold4, untransfold4,

    -- * Level-5
    -- ** trans-roll
    transfold5, untransfold5,

    -- * Level-2 example
    -- $Example_Level2

) where

import DeepControl.Applicative
import DeepControl.Monad

import Control.Monad.Trans
import Control.Monad.Identity (Identity(..))
import Control.Monad.Trans.Identity (IdentityT(..))
import Control.Monad.Trans.List (ListT(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Except (Except, ExceptT(..), runExcept, runExceptT)
import Control.Monad.Writer (Writer, WriterT(..), runWriter)
import Data.Monoid 
-- import Control.Monad.Reader (Reader, ReaderT(..), runReader)

-- $setup
-- >>> import Control.Monad.Trans.Maybe
-- >>> import Control.Monad.List
-- >>> import Control.Monad.Except
-- >>> import Control.Monad.Writer

----------------------------------------------------------------------
-- Level-1

-- | Required only for transfold
class (Monad m, MonadTrans t) => MonadTrans_ m t | m -> t, t -> m where
    trans :: (Monad n) => n (m a) -> t n a
    untrans :: (Monad n) => t n a -> n (m a)

instance MonadTrans_ Identity IdentityT where
    trans   = IdentityT . (runIdentity|$>)
    untrans = (Identity|$>) . runIdentityT
instance MonadTrans_ [] ListT where
    trans   = ListT
    untrans = runListT
instance MonadTrans_ Maybe MaybeT where
    trans   = MaybeT
    untrans = runMaybeT
instance MonadTrans_ (Except e) (ExceptT e) where
    trans x   = ExceptT ((runIdentity . runExceptT) |$> x)
    untrans x = (ExceptT . Identity) |$> runExceptT x
instance (Monoid w) => MonadTrans_ (Writer w) (WriterT w) where
    trans x   = WriterT ((runIdentity . runWriterT) |$> x)
    untrans x = (WriterT . Identity) |$> runWriterT x

{- 
instance MonadTrans_ (Reader r) (ReaderT r) where
    trans x   = ReaderT . sink $ (((runIdentity|$>) . runReaderT) |$> x)
    untrans x = (ReaderT . (Identity|$>)) |$> (sink . runReaderT) x      -- error: Could not deduce (Traversable ((->) r))
-}

----------------------------------------------------------------------
-- Level-2

-- | 
--
-- >>> transfold2 $ [Just 1]
-- MaybeT [Just 1]
--
-- >>> transfold2 $ Just [1]
-- ListT (Just [1])
--
transfold2 :: (Monad m1, MonadTrans_ m2 t2) => m1 (m2 a) -> (t2 m1) a
transfold2 = trans

-- | 
--
-- >>> untransfold2 $ MaybeT [Just 1]
-- [Just 1]
--
-- >>> untransfold2 $ ListT (Just [1])
-- Just [1]
--
untransfold2 :: (Monad m1, MonadTrans_ m2 t2) => (t2 m1) a -> m1 (m2 a)
untransfold2 = untrans

----------------------------------------------------------------------
-- Level-3

-- | 
--
-- >>> transfold3 $ ExceptT (Identity (Right [Just 1]))
-- MaybeT (ListT (ExceptT (Identity (Right [Just 1]))))
--
transfold3 :: (Monad m1, Monad (t2 m1), MonadTrans_ m2 t2, MonadTrans_ m3 t3) => m1 (m2 (m3 a)) -> t3 (t2 m1) a
transfold3 = trans . trans
-- | 
--
-- >>> untransfold3 $ MaybeT (ListT (ExceptT (Identity (Right [Just 1]))))
-- ExceptT (Identity (Right [Just 1]))
--
untransfold3 :: (Monad m1, Monad (t2 m1), MonadTrans_ m2 t2, MonadTrans_ m3 t3) => t3 (t2 m1) a -> m1 (m2 (m3 a))
untransfold3 = untrans . untrans

----------------------------------------------------------------------
-- Level-4

transfold4 :: (Monad m1, Monad (t2 m1), Monad (t3 (t2 m1)), MonadTrans_ m2 t2, MonadTrans_ m3 t3, MonadTrans_ m4 t4) => m1 (m2 (m3 (m4 a))) -> t4 (t3 (t2 m1)) a
transfold4 = trans . trans . trans
untransfold4 :: (Monad m1, Monad (t2 m1), Monad (t3 (t2 m1)), MonadTrans_ m2 t2, MonadTrans_ m3 t3, MonadTrans_ m4 t4) => t4 (t3 (t2 m1)) a -> m1 (m2 (m3 (m4 a)))
untransfold4 = untrans . untrans . untrans

----------------------------------------------------------------------
-- Level-4

transfold5 :: (Monad m1, Monad (t2 m1), Monad (t3 (t2 m1)), Monad (t4 (t3 (t2 m1))), MonadTrans_ m2 t2, MonadTrans_ m3 t3, MonadTrans_ m4 t4, MonadTrans_ m5 t5) => 
              m1 (m2 (m3 (m4 (m5 a)))) -> t5 (t4 (t3 (t2 m1))) a
transfold5 = trans . trans . trans . trans
untransfold5 :: (Monad m1, Monad (t2 m1), Monad (t3 (t2 m1)), Monad (t4 (t3 (t2 m1))), MonadTrans_ m2 t2, MonadTrans_ m3 t3, MonadTrans_ m4 t4, MonadTrans_ m5 t5) => 
                t5 (t4 (t3 (t2 m1))) a -> m1 (m2 (m3 (m4 (m5 a))))
untransfold5 = untrans . untrans . untrans . untrans

----------------------------------------------------------------------
-- Examples

{- $Example_Level2
Here is a monad transformer example how to implement Ackermann function improved to stop within a certain limit of time, with ReaderT-IdentityT2-IO-Maybe monad, a level-2 monad-transformation.

>import DeepControl.Applicative
>import DeepControl.Traversable (sink)
>import DeepControl.Monad ((>-))
>import DeepControl.Monad.Morph ((|*|), (|>|))
>import DeepControl.Monad.Trans (transfold2, untransfold2)
>import DeepControl.Monad.Trans.Identity (Identity(..), IdentityT(..), IdentityT2(..))
>import Control.Monad.Reader
>import Control.Monad.Trans.Maybe
>
>import System.Timeout (timeout)
>
>type TimeLimit = Int
>
>ackermannTimeLimit :: TimeLimit -> Int -> Int -> 
>                      IO (Maybe Int)                      -- IO-Maybe Monad
>ackermannTimeLimit timelimit x y = timeout timelimit (ackermannIO x y)
>  where
>    ackermannIO :: Int -> Int -> IO Int
>    ackermannIO 0 n = (.*) $ n + 1
>    ackermannIO m n | m > 0 && n == 0 = ackermannIO (m-1) 1
>                    | m > 0 && n > 0  = ackermannIO m (n-1) >>= ackermannIO (m-1)
> 
>ackermann :: Int -> Int -> 
>             ReaderT TimeLimit (IdentityT2 IO Maybe) Int  -- ReaderT-IdentityT2-IO-Maybe monad
>ackermann x y = do
>    timelimit <- ask
>    (|*|) . IdentityT2 $ ackermannTimeLimit timelimit x y -- lift IO-Maybe function to ReaderT-IdentityT2-IO-Maybe function
>
>calc_ackermann :: TimeLimit -> Int -> Int -> IO (Maybe Int)
>calc_ackermann timelimit x y = ackermann x y >- \r -> runReaderT r timelimit
>                                             >- runIdentityT2
>
>-- λ> sink $ calc_ackermann 1000 |$> [0..4] |* 4
>-- [Just 5,Just 6,Just 11,Just 125,Nothing]
>
>ackermann' :: Int -> Int -> 
>              ReaderT TimeLimit (MaybeT IO) Int                 -- ReaderT-MaybeT-IO monad
>ackermann' x y = (transfold2 . runIdentityT2) |>| ackermann x y -- You can get usual ReaderT-MaybeT-IO function from ReaderT-IdentityT2-IO-Maybe function
>
>ackermann'' :: Int -> Int -> 
>               ReaderT TimeLimit (IdentityT2 IO Maybe) Int       -- ReaderT-IdentityT2-IO-Maybe monad
>ackermann'' x y = (IdentityT2 . untransfold2) |>| ackermann' x y -- You can get ReaderT-IdentityT2-IO-Maybe function from usual ReaderT-MaybeT-IO function
-}


