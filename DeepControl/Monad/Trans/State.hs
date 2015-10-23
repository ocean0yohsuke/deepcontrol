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
--{-# LANGUAGE TypeSynonymInstances #-}
--{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DeepControl.Monad.Trans.State (
    module Control.Monad.State,

    -- * Level-2
    -- StateT2(..), runStateT2,

    ) where 

import DeepControl.Applicative
import DeepControl.Monad.Trans

import Control.Monad.State 

-- import DeepControl.Monad
-- import DeepControl.Monad.Trans.Identity

----------------------------------------------------------------------
-- Level-1

instance MonadTransDown (StateT s) where
    type TransDown (StateT s) = State s

instance MonadTransCover (StateT s) where
    (|*|) = StateT . ((*:)|$>) . runState

----------------------------------------------------------------------
-- Level-2

{-
newtype StateT2 s m1 m2 a = StateT2 (StateT s (IdentityT2 m1 m2) a)
  deriving (Functor, Applicative, Monad, MonadIO)

stateT2 :: (s -> m1 (m2 (a, s))) -> StateT2 s m1 m2 a
stateT2 = StateT2 . StateT . (IdentityT2|$>)
runStateT2 :: StateT2 s m1 m2 a -> s -> m1 (m2 (a, s))
runStateT2 (StateT2 x) = (runIdentityT2|$>) . runStateT $ x 

instance MonadTrans2 (StateT2 s) where
    lift2 x = stateT2 $ \s -> x <<$|(,)|** s

instance MonadTrans2Down (StateT2 s) where
    type Trans2Down (StateT2 s) = StateT s

instance MonadTransFold2 (StateT2 s) where
    transfold2 x = StateT $ trans |$> runStateT2 x
    untransfold2 x = stateT2 $ untrans |$> runStateT x

instance MonadTransCover2 (StateT2 s) where
    (|-*|) = stateT2 . ((-*)|$>) . runStateT
    (|*-|) = stateT2 . ((*-)|$>) . runStateT
-}

{-
type StateT2 s m1 m2 a = StateT s (IdentityT2 m1 m2) a

stateT2 :: (s -> m1 (m2 (a, s))) -> StateT2 s m1 m2 a
stateT2 = StateT . (IdentityT2|$>)
runStateT2 :: StateT2 s m1 m2 a -> s -> m1 (m2 (a, s))
runStateT2 = (runIdentityT2|$>) . runStateT

instance MonadTrans2Down (StateT2 s) where
    type Trans2Down (StateT2 s) = StateT s

-- TODO: error
instance MonadTransFold2 (StateT2 s) where
    transfold2 x = StateT $ trans |$> runStateT2 x
    untransfold2 x = stateT2 $ untrans |$> runStateT x

instance MonadTransCover2 (StateT2 s) where
    (|-*|) = stateT2 . ((-*)|$>) . runStateT
    (|*-|) = stateT2 . ((*-)|$>) . runStateT
-}


