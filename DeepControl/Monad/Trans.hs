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
    trans,
    MonadTrans(..),
    MonadTrans2(..),
    MonadTrans3(..),
    MonadTrans4(..),
    MonadTrans5(..),

    -- * Level-2 example
    -- $Example_Level2

) where

import           DeepControl.Monad

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class (MonadTrans (..))

----------------------------------------------------------------------
-- Level-1

-- | Alias for @'Control.Monad.Trans.Class.lift'@.
trans :: (Monad m, MonadTrans t) => m a -> t m a
trans = lift

----------------------------------------------------------------------
-- Level-2

class  MonadTrans2 t  where
    trans2 :: (Monad m1, Monad2 m2) => m1 (m2 a) -> t m1 m2 a

----------------------------------------------------------------------
-- Level-3

class  MonadTrans3 t  where
    trans3 :: (Monad m1, Monad2 m2, Monad3 m3) => m1 (m2 (m3 a)) -> t m1 m2 m3 a

----------------------------------------------------------------------
-- Level-4

class  MonadTrans4 t  where
    trans4 :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m1 (m2 (m3 (m4 a))) -> t m1 m2 m3 m4 a

----------------------------------------------------------------------
-- Level-5

class  MonadTrans5 t  where
    trans5 :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m1 (m2 (m3 (m4 (m5 a)))) -> t m1 m2 m3 m4 m5 a

----------------------------------------------------------------------
-- Examples

{- $Example_Level2
Here is a monad transformer example how to implement Ackermann function, improved to stop within a certain limit of time, with ReaderT2-IO-Maybe monad, a level-2 monad-transformation.

>import DeepControl.Applicative
>import DeepControl.Commutative (commute)
>import DeepControl.Monad ((>-))
>import DeepControl.Monad.Trans (trans2)
>import DeepControl.Monad.Trans.Reader
>
>import System.Timeout (timeout)
>
>type TimeLimit = Int
>
>ackermann :: TimeLimit -> Int -> Int ->
>             IO (Maybe Int)                        -- IO-Maybe Monad
>ackermann timelimit x y = timeout timelimit (ackermannIO x y)
>  where
>    ackermannIO :: Int -> Int -> IO Int
>    ackermannIO 0 n = (*:) $ n + 1
>    ackermannIO m n | m > 0 && n == 0 = ackermannIO (m-1) 1
>                    | m > 0 && n > 0  = ackermannIO m (n-1) >>= ackermannIO (m-1)
>
>ackermannR :: Int -> Int ->
>              ReaderT2 TimeLimit IO Maybe Int      -- ReaderT2-IO-Maybe monad
>ackermannR x y = do
>    timelimit <- ask
>    trans2 $ ackermann timelimit x y               -- transform(lift) IO-Maybe function to ReaderT2-IO-Maybe function
>
>calc_ackermann :: TimeLimit -> Int -> Int -> IO (Maybe Int)
>calc_ackermann timelimit x y = ackermannR x y >- \r -> runReaderT2 r timelimit
>
>-- λ> commute $ calc_ackermann 1000 |$> [0..4] |* 4
>-- [Just 5,Just 6,Just 11,Just 125,Nothing]
-}
