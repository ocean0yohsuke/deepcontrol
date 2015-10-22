{-|
Module      : DeepControl.Monad.Except
Description : Extension for mtl's Contrl.Monad.Except.
Copyright   : (C) 2013 Ross Paterson,
              (C) 2015 KONISHI Yohsuke 
License     : BSD-style (see the file LICENSE)
Maintainer  : ocean0yohsuke@gmail.com
Stability   : experimental
Portability : ---

This module extended RWS Monad of mtl(monad-transformer-library).
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module DeepControl.Monad.Trans.Except (
    -- * Level-1
    module Control.Monad.Except,

    -- * Level-2
    throwError2,
    catchError2,

    -- * Level-3
    throwError3,
    catchError3,

    -- * Level-4
    throwError4,
    catchError4,

    -- * Level-5
    throwError5,
    catchError5,

    ) where 

import DeepControl.Applicative
import DeepControl.Monad
import DeepControl.Commutative

import Control.Monad.Except
import Control.Monad.Identity

----------------------------------------------------------------
-- Level-1

instance Commutative (Except e) where
    commute x = ExceptT . Identity |$> commute (runExcept $ x)

instance Monad2 (Except e) where
    m >>== f = (ExceptT . Identity |$>) $ (runExcept |$> m) >>== runExcept |$>> f
instance Monad3 (Except e) where
    m >>>== f = (ExceptT . Identity |$>>) $ (runExcept |$>> m) >>>== runExcept |$>>> f
instance Monad4 (Except e) where
    m >>>>== f = (ExceptT . Identity |$>>>) $ (runExcept |$>>> m) >>>>== runExcept |$>>>> f
instance Monad5 (Except e) where
    m >>>>>== f = (ExceptT . Identity |$>>>>) $ (runExcept |$>>>> m) >>>>>== runExcept |$>>>>> f

----------------------------------------------------------------
-- Level-2

throwError2 :: (MonadError e m2, Applicative m1) => e -> m1 (m2 a)
throwError2 = (*:) |$> throwError

catchError2 :: (MonadError e m2, Commutative m1, Commutative m2) => 
                m1 (m2 a) -> (e -> m1 (m2 a)) -> m1 (m2 a)
catchError2 = coverCatch catchError
  where
    coverCatch :: (Commutative m1, Commutative m2) => 
                  (forall a. m2 a -> (e -> m2 a) -> m2 a) -> 
                   m1 (m2 a) -> (e -> m1 (m2 a)) -> m1 (m2 a)
    coverCatch catch m h = 
        let m' = commute m
            h' = commute |$> h
        in commute $ catch m' h'

----------------------------------------------------------------
-- Level-3

throwError3 :: (MonadError e m3, Applicative m1, Applicative m2) => e -> m1 (m2 (m3 a))
throwError3 = (**:) |$> throwError

catchError3 :: (MonadError e m3, Commutative m1, Commutative m2, Commutative m3) => 
                m1 (m2 (m3 a)) -> (e -> m1 (m2 (m3 a))) -> m1 (m2 (m3 a))
catchError3 = cover2Catch catchError
  where
    cover2Catch :: (Commutative m1, Commutative m2, Commutative m3) => 
                   (forall a. m3 a -> (e -> m3 a) -> m3 a) -> 
                    m1 (m2 (m3 a)) -> (e -> m1 (m2 (m3 a))) -> m1 (m2 (m3 a))
    cover2Catch catch m h = 
        let m' = float2 m
            h' = float2 |$> h
        in sink2 $ catch m' h'

----------------------------------------------------------------
-- Level-4

throwError4 :: (MonadError e m4, Applicative m1, Applicative m2, Applicative m3) => e -> m1 (m2 (m3 (m4 a)))
throwError4 = (***:) |$> throwError

catchError4 :: (MonadError e m4, Commutative m1, Commutative m2, Commutative m3, Commutative m4) => 
                m1 (m2 (m3 (m4 a))) -> (e -> m1 (m2 (m3 (m4 a)))) -> m1 (m2 (m3 (m4 a)))
catchError4 = cover3Catch catchError
  where
    cover3Catch :: (Commutative m1, Commutative m2, Commutative m3, Commutative m4) => 
                   (forall a. m4 a -> (e -> m4 a) -> m4 a) -> 
                    m1 (m2 (m3 (m4 a))) -> (e -> m1 (m2 (m3 (m4 a)))) -> m1 (m2 (m3 (m4 a)))
    cover3Catch catch m h = 
        let m' = float3 m
            h' = float3 |$> h
        in sink3 $ catch m' h'

----------------------------------------------------------------
-- Level-5

throwError5 :: (MonadError e m5, Applicative m1, Applicative m2, Applicative m3, Applicative m4) => e -> m1 (m2 (m3 (m4 (m5 a))))
throwError5 = (****:) |$> throwError

catchError5 :: (MonadError e m5, Commutative m1, Commutative m2, Commutative m3, Commutative m4, Commutative m5) => 
                m1 (m2 (m3 (m4 (m5 a)))) -> (e -> m1 (m2 (m3 (m4 (m5 a))))) -> m1 (m2 (m3 (m4 (m5 a))))
catchError5 = cover4Catch catchError
  where
    cover4Catch :: (Commutative m1, Commutative m2, Commutative m3, Commutative m4, Commutative m5) => 
                   (forall a. m5 a -> (e -> m5 a) -> m5 a) -> 
                    m1 (m2 (m3 (m4 (m5 a)))) -> (e -> m1 (m2 (m3 (m4 (m5 a))))) -> m1 (m2 (m3 (m4 (m5 a))))
    cover4Catch catch m h = 
        let m' = float4 m
            h' = float4 |$> h
        in sink4 $ catch m' h'

----------------------------------------------------------------
-- Level-6

{-
cover5Catch :: (Commutative m1, Commutative m2, Commutative m3, Commutative m4, Commutative m5, Commutative m6) => 
               (forall a. m6 a -> (e -> m6 a) -> m6 a) -> 
                m1 (m2 (m3 (m4 (m5 (m6 a))))) -> (e -> m1 (m2 (m3 (m4 (m5 (m6 a)))))) -> m1 (m2 (m3 (m4 (m5 (m6 a)))))
cover5Catch catch m h = 
    let m' = float5 m
        h' = float5 |$> h
    in sink5 $ catch m' h'
-}
