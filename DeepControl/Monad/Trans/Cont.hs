{-|
Module      : DeepControl.Monad.Trans.Cont
Description : Extension for mtl's Contrl.Monad.Cont.
Copyright   : (c) The University of Glasgow 2001,
              (c) Jeff Newbern 2003-2007,
              (c) Andriy Palamarchuk 2007,
              (C) 2015 KONISHI Yohsuke,
License     : BSD-style (see the file LICENSE)
Maintainer  : ocean0yohsuke@gmail.com
Stability   : experimental
Portability : ---

This module extended Cont monad of mtl(monad-transformer-library).
-}
{-# LANGUAGE TypeFamilies #-}
module DeepControl.Monad.Trans.Cont (
    module Control.Monad.Cont,

    ) where 

import DeepControl.Applicative
import DeepControl.Monad ((>-))
import DeepControl.Monad.Trans

import Control.Monad.Cont 

----------------------------------------------------------------------
-- Level-1


instance MonadTransDown (ContT r) where
    type TransDown (ContT r) = Cont r

{-
instance MonadTransCover (ContT r) where
    (|*|) m = ContT $ \k -> (runCont m) k

-- liftCont :: Monad m => (forall r. (a -> r) -> r) -> ((a -> m r) -> m r)
-- liftCont f g = 
-}


