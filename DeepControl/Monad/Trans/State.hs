{-# LANGUAGE TypeFamilies #-}
{-|
Module      : DeepControl.Monad.Trans.State
Description : Extension for mtl's Contrl.Monad.State.
Copyright   : (c) Andy Gill 2001,
              (c) Oregon Graduate Institute of Science and Technology, 2001,
              (C) 2015 KONISHI Yohsuke,
License     : BSD-style (see the file LICENSE)
Maintainer  : ocean0yohsuke@gmail.com
Stability   : experimental
Portability : ---

This module extended State monad of mtl(monad-transformer-library).
-}
{-# LANGUAGE TypeFamilies #-}
module DeepControl.Monad.Trans.State (
    module Control.Monad.State,

    ) where 

import DeepControl.Applicative
import DeepControl.Monad.Trans

import Control.Monad.State 

----------------------------------------------------------------------
-- Level-1

instance MonadTransDown (StateT s) where
    type TransDown (StateT s) = State s

instance MonadTransCover (StateT s) where
    (|*|) = StateT . ((*:)|$>) . runState


