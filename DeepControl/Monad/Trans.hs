{-|
Module      : DeepControl.Monad.Trans
Description : Enable deep level Monad-Transform programming.
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
    -- * Level-1
    -- ** trans-cover
    (|*|),

    -- * Level-2
    -- ** trans-cover  
    (|**|),
    (|-*|), 

    -- * Level-3
    -- ** trans-cover  
    (|***|),
    (|--*|), 
    (|-**|), (|*-*|), 

    -- * Level-4
    -- ** trans-cover
    (|****|),
    (|---*|),
    (|--**|), (|-*-*|), (|*--*|),
    (|-***|), (|*-**|), (|**-*|), 


    -- * Level-5
    -- ** trans-cover
    (|*****|),
    (|----*|), 
    (|---**|), (|--*-*|), (|-*--*|), (|*---*|), 
    (|--***|), (|-*-**|), (|*--**|), (|*-*-*|), (|-**-*|), (|**--*|),
    (|-****|), (|*-***|), (|**-**|), (|***-*|), 

    -- * MonadTrans
    MonadTrans(..), 
    MonadTrans_(..), 

    -- * MonadIO
    MonadIO(..),

    -- * Level-2 example
    -- $Example_Level2

    -- * Level-2 example2
    -- $Example_Level2_cover

) where

import DeepControl.Applicative
import DeepControl.Monad
import DeepControl.Monad.Morph

import Control.Monad.IO.Class
import Control.Monad.Trans.Class (MonadTrans (..))
import qualified Control.Monad.List          as L
import qualified Control.Monad.Trans.Maybe   as M
import qualified Control.Monad.Except        as E
import Control.Monad.Identity
import Control.Monad.Trans.Identity
import Control.Monad.RWS (RWS, RWST(..), runRWS)
import Control.Monad.Reader (Reader, ReaderT(..), runReader)
import Control.Monad.State (State, StateT(..), runState)
import Control.Monad.Writer (Writer, WriterT(..), runWriter)
import Data.Monoid 

----------------------------------------------------------------------
-- Level-1

infixl 3 |*|
-- | Alias to @'lift'@
(|*|) :: (Monad m, MonadTrans t) => m a -> t m a 
(|*|) = lift

-- | Required only for transfold
class (Monad m, MonadTrans t) => MonadTrans_ m t | m -> t, t -> m where
    trans :: (Monad n) => n (m a) -> t n a
    untrans :: (Monad n) => t n a -> n (m a)

instance MonadTrans_ Identity IdentityT where
    trans   = IdentityT . (runIdentity|$>)
    untrans = (Identity|$>) . runIdentityT
instance MonadTrans_ [] L.ListT where
    trans   = L.ListT
    untrans = L.runListT
instance MonadTrans_ Maybe M.MaybeT where
    trans   = M.MaybeT
    untrans = M.runMaybeT
instance MonadTrans_ (E.Except e) (E.ExceptT e) where
    trans x   = E.ExceptT ((runIdentity . E.runExceptT) |$> x)
    untrans x = (E.ExceptT . Identity) |$> E.runExceptT x
instance (Monoid w) => MonadTrans_ (Writer w) (WriterT w) where
    trans x   = WriterT ((runIdentity . runWriterT) |$> x)
    untrans x = (WriterT . Identity) |$> runWriterT x

----------------------------------------------------------------------
-- Level-2

infixl 3 |**|
(|**|) :: (Monad m, MonadTrans t1, MonadTrans t2, Monad (t2 m)) => m a -> t1 (t2 m) a 
(|**|) = (|*|) . (|*|)

infixl 3  |-*|
(|-*|) :: (Monad m, MonadTrans t1, MonadTrans t2, MFunctor t1) => t1 m a -> t1 (t2 m) a
(|-*|) = ((|*|) |>|)

----------------------------------------------------------------------
-- Level-3

infixl 3 |***|
(|***|) :: (Monad m, Monad (t2 (t3 m)), Monad (t3 m),
            MonadTrans t1, MonadTrans t2, MonadTrans t3) => 
            m a -> t1 (t2 (t3 m)) a 
(|***|) = (|*|) . (|**|)

infixl 3  |--*|
(|--*|) :: (Monad m, Monad (t2 m),
            MonadTrans t1, MonadTrans t2, MonadTrans t3, 
            MFunctor t1, MFunctor t2) => 
            t1 (t2 m) a -> t1 (t2 (t3 m)) a
(|--*|) = ((|*|) |>>|)

infixl 3  |-**|, |*-*|
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

----------------------------------------------------------------------
-- Level-4

infixl 3 |****|
(|****|) :: (Monad m, Monad (t2 (t3 (t4 m))), Monad (t3 (t4 m)), Monad (t4 m),
            MonadTrans t1, MonadTrans t2, MonadTrans t3, MonadTrans t4) => 
            m a -> t1 (t2 (t3 (t4 m))) a 
(|****|) = (|*|) . (|***|)

infixl 3  |---*|
(|---*|) :: (Monad m, Monad (t2 (t3 m)), Monad (t3 m), 
            MFunctor t1, MFunctor t2, MFunctor t3,
            MonadTrans t1, MonadTrans t2, MonadTrans t3, MonadTrans t4) => 
            t1 (t2 (t3 m)) a -> t1 (t2 (t3 (t4 m))) a
(|---*|) = ((|*|) |>>>|)
infixl 3  |--**|, |-*-*|
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
infixl 3  |-***|, |*-**|, |**-*|
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

----------------------------------------------------------------------
-- Level-4

infixl 3 |*****|
(|*****|) :: (Monad m, Monad (t2 (t3 (t4 (t5 m)))), Monad (t3 (t4 (t5 m))), Monad (t4 (t5 m)), Monad (t5 m), 
              MonadTrans t1, MonadTrans t2, MonadTrans t3, MonadTrans t4, MonadTrans t5) => 
              m a -> t1 (t2 (t3 (t4 (t5 m)))) a 
(|*****|) = (|*|) . (|****|)

infixl 3  |----*|
(|----*|) :: (Monad m, Monad (t2 (t3 (t4 m))), Monad (t3 (t4 m)), Monad (t4 m), 
              MFunctor t1, MFunctor t2, MFunctor t3, MFunctor t4, 
              MonadTrans t1, MonadTrans t2, MonadTrans t3, MonadTrans t4, MonadTrans t5) => 
              t1 (t2 (t3 (t4 m))) a -> t1 (t2 (t3 (t4 (t5 m)))) a
(|----*|) = ((|*|) |>>>>|)

infixl 3  |---**|, |--*-*|, |-*--*|, |*---*|
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

infixl 3  |--***|, |-*-**|, |*--**|, |*-*-*|, |-**-*|, |**--*|
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

infixl 3  |-****|, |*-***|, |**-**|, |***-*|
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

{- $Example_Level2
Here is a monad transformer example how to implement Ackermann function, improved to stop within a certain limit of time, with ReaderT-IdentityT2-IO-Maybe monad, a level-2 monad-transformation.

>import DeepControl.Applicative
>import DeepControl.Commutative (commute)
>import DeepControl.Monad ((>-))
>import DeepControl.Monad.Morph ((|>|))
>import DeepControl.Monad.Trans ((|*|))
>import DeepControl.Monad.Trans.Identity (Identity(..), IdentityT(..), IdentityT2(..), transfold2, untransfold2)
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
>import DeepControl.Commutative (Commutative)
>import DeepControl.Monad (Monad)
>import DeepControl.Monad.Morph (generalize, (|>|))
>import DeepControl.Monad.Trans ((|*|))
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
>    ((-*:) . IdentityT) |>| tock                                                    -- (-*:) is a level-2 trans-cover function, analogous to (-*)
>        :: (Monad m, Commutative m) => StateT Int (IdentityT2 IO m             ) ()
>    ((*-:) . IdentityT) |>| save                                                    -- (*-:) is a level-2 trans-cover function, analogous to (.*)
>        :: (Monad m               ) => StateT Int (IdentityT2 m  (Writer [Int])) ()
>
>-- λ> execWriter |$> runIdentityT2 (runStateT program 0)
>-- Tock!
>-- Tock!
>-- Tock!
>-- Tock!
>-- [1,2,3,4]
-}
