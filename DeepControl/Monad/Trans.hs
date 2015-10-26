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
    -- ** trans-lift
    MonadTrans(..), 
    liftT,
    -- ** other  
    MonadTrans_(..), 

    -- * Level-2
    -- ** trans-lift
    MonadTrans2(..), 

    -- * Level-3
    -- ** trans-lift
    MonadTrans3(..), 

    -- * Level-4
    -- ** trans-lift
    MonadTrans4(..),

    -- * Level-5
    -- ** trans-lift
    MonadTrans5(..),

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
import Control.Monad.Trans.Identity
import Control.Monad.RWS (RWS, RWST(..), runRWS)
import Control.Monad.Reader (Reader, ReaderT(..), runReader)
import Control.Monad.State (State, StateT(..), runState)
import Control.Monad.Writer (Writer, WriterT(..), runWriter)
import Data.Monoid 

----------------------------------------------------------------------
-- Level-1

-- | Alias to @'lift'@
liftT :: (Monad m, MonadTrans t) => m a -> t m a 
liftT = lift

-- | Required only for @'MonadTransFold2'@ and @'MonadTransFold3'@ 
class (Monad m, MonadTrans t) => MonadTrans_ m t | m -> t where
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

class MonadTrans2 t where
    liftT2 :: (Monad m1, Monad m2) => m1 (m2 a) -> t m1 m2 a

----------------------------------------------------------------------
-- Level-3

class MonadTrans3 t where
    liftT3 :: (Monad m1, Monad m2, Monad m3) => m1 (m2 (m3 a)) -> t m1 m2 m3 a

----------------------------------------------------------------------
-- Level-4

class  MonadTrans4 t  where
    liftT4 :: (Monad m1, Monad m2, Monad m3, Monad m4) => m1 (m2 (m3 (m4 a))) -> t m1 m2 m3 m4 a

----------------------------------------------------------------------
-- Level-5

class MonadTrans5 t where
    liftT5 :: (Monad m1, Monad m2, Monad m3, Monad m4, Monad m5) => m1 (m2 (m3 (m4 (m5 a)))) -> t m1 m2 m3 m4 m5 a

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
>import DeepControl.Commutative (Commutative)
>import DeepControl.Monad (Monad)
>import DeepControl.Monad.Morph (generalize, (|>|))
>import DeepControl.Monad.Trans (liftT)
>import DeepControl.Monad.Trans.Identity (IdentityT2(..), (|-*|), (|*-|))
>import Control.Monad.Writer
>import Control.Monad.State
>
>tick :: State Int ()
>tick = modify (+1)
>
>tock                         ::                   StateT Int IO ()
>tock = do
>    generalize |>| tick      :: (Monad      m) => StateT Int m  ()  -- (|>|) is the level-1 trans-map function, analogous for (|$>)
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
>program ::                             StateT Int (IdentityT2 IO (Writer [Int])) () -- StateT-IdentityT2-IO-Writer monad, a level-2 monad-transform
>program = replicateM_ 4 $ do
>    ((|-*|).liftT) |>| tock                                                         -- (|-*|) is a level-2 trans-cover function, analogous for (-*)
>        :: (Monad m, Commutative m) => StateT Int (IdentityT2 IO m             ) ()
>    ((|*-|).liftT) |>| save                                                         -- (|*-|) is a level-2 trans-cover function, analogous for (*:)
>        :: (Monad m               ) => StateT Int (IdentityT2 m  (Writer [Int])) ()
>
>-- λ> execWriter |$> runIdentityT2 (runStateT program 0)
>-- Tock!
>-- Tock!
>-- Tock!
>-- Tock!
>-- [1,2,3,4]
-}
