{-|
Module      : DeepControl.MonadTrans
Description : Enable deep level Monad-Transform programming.
Copyright   : (C) 2015 KONISHI Yohsuke 
License     : BSD-style (see the LICENSE file in the distribution)
Maintainer  : ocean0yohsuke@gmail.com
Stability   : experimental
Portability : ---

-}
module DeepControl.MonadTrans (
    MonadIO(..),

    MonadTrans(..), 
    MonadTrans2(..),
    MonadTrans3(..),
    MonadTrans4(..),
    MonadTrans5(..),
) where

import DeepControl.Monad

import Control.Monad.IO.Class

----------------------------------------------------------------------
-- Level-1

class  MonadTrans t  where
    trans :: (Monad m) => m a -> t m a

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


