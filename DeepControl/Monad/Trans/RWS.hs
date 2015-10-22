{-|
Module      : DeepControl.Monad.Trans.RWS
Description : Extension for mtl's Contrl.Monad.RWS.
Copyright   : (C) 2015 KONISHI Yohsuke,
              (c) Andy Gill 2001,
              (c) Oregon Graduate Institute of Science and Technology, 2001
License     : BSD-style (see the file LICENSE)
Maintainer  : ocean0yohsuke@gmail.com
Stability   : experimental
Portability : ---

This module extended RWS monad in mtl(monad-transformer-library).
-}
{-# LANGUAGE TypeFamilies #-}
module DeepControl.Monad.Trans.RWS (
    module Control.Monad.RWS,
    MonadReader(..), MonadWriter(..), MonadState(..),

    ) where 

import DeepControl.Applicative
import DeepControl.Monad.Trans

import Control.Monad.Reader (MonadReader(..))
import Control.Monad.Writer (MonadWriter(..))
import Control.Monad.State (MonadState(..))
import Control.Monad.RWS
import Data.Monoid

----------------------------------------------------------------------
-- Level-1

instance (Monoid w) => MonadTransDown (RWST r w s) where
    type TransDown (RWST r w s) = RWS r w s

instance (Monoid w) => MonadTransCover (RWST r w s) where
    (|*|) = RWST . ((*:)|$>>) . runRWS


