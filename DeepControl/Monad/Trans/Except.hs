{-|
Module      : DeepControl.Monad.Except
Description : Extension for mtl's Contrl.Monad.Except.
Copyright   : (C) 2013 Ross Paterson,
              (C) 2015 KONISHI Yohsuke 
License     : BSD-style (see the file LICENSE)
Maintainer  : ocean0yohsuke@gmail.com
Stability   : experimental
Portability : ---

This module extended RWS Monad in mtl(monad-transformer-library).
-}
{-# LANGUAGE 
        DeriveFunctor,
        GeneralizedNewtypeDeriving,
        FlexibleInstances,
        FunctionalDependencies
 #-}
module DeepControl.Monad.Trans.Except (
    module Control.Monad.Except,

    ) where 

import DeepControl.Applicative
import DeepControl.Monad
-- import DeepControl.Monad.Trans
import DeepControl.Commutative

import Control.Monad.Except
import Control.Monad.Identity

----------------------------------------------------------------
--

instance Monad2 (Except e) where
    m >>== f = (ExceptT . Identity |$>) $ (runExcept |$> m) >>== runExcept |$>> f
instance Monad3 (Except e) where
    m >>>== f = (ExceptT . Identity |$>>) $ (runExcept |$>> m) >>>== runExcept |$>>> f
instance Monad4 (Except e) where
    m >>>>== f = (ExceptT . Identity |$>>>) $ (runExcept |$>>> m) >>>>== runExcept |$>>>> f
instance Monad5 (Except e) where
    m >>>>>== f = (ExceptT . Identity |$>>>>) $ (runExcept |$>>>> m) >>>>>== runExcept |$>>>>> f

instance Commutative (Except e) where
    commute x = ExceptT . Identity |$> commute (runExcept $ x)

