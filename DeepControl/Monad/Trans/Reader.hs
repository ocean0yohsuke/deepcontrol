{-|
Module      : DeepControl.Monad.Trans.Reader
Description : Extension for mtl's Contrl.Monad.Reader.
Copyright   : (c) Andy Gill 2001,
              (c) Oregon Graduate Institute of Science and Technology 2001,
              (c) Jeff Newbern 2003-2007,
              (c) Andriy Palamarchuk 2007,
              (C) 2015 KONISHI Yohsuke,
License     : BSD-style (see the file LICENSE)
Maintainer  : ocean0yohsuke@gmail.com
Stability   : experimental
Portability : ---

This module extended Reader monad of mtl(monad-transformer-library).
-}
{-# LANGUAGE TypeFamilies #-}
module DeepControl.Monad.Trans.Reader (
    module Control.Monad.Reader,
   
    ) where 

import DeepControl.Applicative
import DeepControl.Monad.Trans

import Control.Monad.Reader 

----------------------------------------------------------------------
-- Level-1

instance MonadTransDown (ReaderT r) where
    type TransDown (ReaderT r) = Reader r

instance MonadTransCover (ReaderT s) where
    (|*|) = ReaderT . ((*:)|$>) . runReader


