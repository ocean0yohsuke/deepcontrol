{-|
Module      : DeepControl.Monad.Maybe
Description : 
Copyright   : (c) 2007 Yitzak Gale, Eric Kidd,
              (C) 2015 KONISHI Yohsuke
License     : BSD-style (see the file LICENSE)
Maintainer  : ocean0yohsuke@gmail.com
Stability   : experimental
Portability : ---

This module is just a concise mimic for Maybe Monad in mtl(monad-transformer-library).
The qualifier "concise" means that this module doesn't make no attempt to transform functions of any kind of Monad automatically.
So when making some new data type of MaybeT, you have to manually define involved Monad instances, 
for example `DeepControl.Monad.MonadReader`, `DeepControl.Monad.MonadWriter` or `DeepControl.Monad.MonadState`, 
by making use of the transformation functions such as `trans`, `trans2`, etc.
Admittedly it is tedious though, you can deeply understand monad-transformation mechanism instead.
-}
module DeepControl.Monad.Maybe (

    -- * Level-1
    MaybeT(..), mapMaybeT, liftCatch,

    ) where 

import DeepControl.Applicative
import DeepControl.Monad
import DeepControl.MonadTrans
import DeepControl.Commutative

import Control.Monad.Signatures

----------------------------------------------------------------------
-- Level-1

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
    fmap f mv = MaybeT $ f |$>> runMaybeT mv
instance (Monad m) => Applicative (MaybeT m) where
    pure = MaybeT . (**:)
    f <*> v = MaybeT $ runMaybeT f |*>> runMaybeT v
instance (Monad m) => Monad (MaybeT m) where
    return = pure
    mv >>= f = MaybeT $ 
        runMaybeT mv >>== \v -> 
        runMaybeT (f v)

instance MonadTrans MaybeT where
    trans = MaybeT . (-*) 
instance (MonadIO m) => MonadIO (MaybeT m) where
    liftIO = trans . liftIO

mapMaybeT :: (m (Maybe a) -> n (Maybe b)) -> MaybeT m a -> MaybeT n b
mapMaybeT f = MaybeT . f . runMaybeT

liftCatch :: Catch e m (Maybe a) -> Catch e (MaybeT m) a
liftCatch catchE m h = MaybeT $ runMaybeT m `catchE` \e -> runMaybeT (h e)

