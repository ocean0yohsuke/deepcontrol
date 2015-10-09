{-|
Module      : DeepControl.Monad.List
Description : 
Copyright   : (c) Andy Gill 2001,
              (c) Oregon Graduate Institute of Science and Technology, 2001,
              (C) 2015 KONISHI Yohsuke 
License     : BSD-style (see the file LICENSE)
Maintainer  : ocean0yohsuke@gmail.com
Stability   : experimental
Portability : ---

This module is just a concise mimic for List Monad in mtl(monad-transformer-library).
The qualifier "concise" means that this module doesn't make no attempt to transform functions of any kind of Monad automatically.
So when making some new data type of ListT, you have to manually define involved Monad instances, 
for example `DeepControl.Monad.MonadReader`, `DeepControl.Monad.MonadWriter` or `DeepControl.Monad.MonadState`, 
by making use of the transformation functions such as `trans`, `trans2`, etc.
Admittedly it is tedious though, you can deeply understand monad-transformation mechanism instead.
-}
module DeepControl.Monad.List (

    -- * Level-1
    ListT(..), mapListT, liftCallCC, liftCatch

    ) where 

import DeepControl.Applicative
import DeepControl.Monad
import DeepControl.MonadTrans

import Control.Monad.Signatures

----------------------------------------------------------------------
-- Level-1

newtype ListT m a = ListT { runListT :: m [a] }

instance (Functor m) => Functor (ListT m) where
    fmap f mv = ListT $ f |$>> runListT mv
instance (Applicative m) => Applicative (ListT m) where
    pure = ListT . (**:)
    f <*> v = ListT $ runListT f |*>> runListT v
instance (Monad m) => Monad (ListT m) where
    return = pure
    mv >>= f = ListT $ 
        runListT mv >>== \v ->
        runListT (f v)

instance MonadTrans ListT where
    trans = ListT . (-*)
instance (MonadIO m) => MonadIO (ListT m) where
    liftIO = trans . liftIO

mapListT :: (m [a] -> n [b]) -> ListT m a -> ListT n b
mapListT f = ListT . f . runListT

liftCallCC :: CallCC m [a] [b] -> CallCC (ListT m) a b
liftCallCC callCC f = ListT $
    callCC $ \c ->
    runListT $ (\a -> ListT $ c [a]) >- f

liftCatch :: Catch e m [a] -> Catch e (ListT m) a
liftCatch catchE m h = ListT $ runListT m `catchE` \e -> runListT (h e)

