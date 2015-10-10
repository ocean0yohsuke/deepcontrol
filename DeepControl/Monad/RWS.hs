{-|
Module      : DeepControl.Monad.RWS
Description : 
Copyright   : (C) 2015 KONISHI Yohsuke,
              (c) Andy Gill 2001,
              (c) Oregon Graduate Institute of Science and Technology, 2001
License     : BSD-style (see the file LICENSE)
Maintainer  : ocean0yohsuke@gmail.com
Stability   : experimental
Portability : ---

This module is just a concise mimic for RWS Monad in mtl(monad-transformer-library).
The qualifier "concise" means that this module doesn't make no attempt to transform functions of any kind of Monad automatically.
So when making some new data type of RWSTx, you have to manually define involved Monad instances, 
for example `DeepControl.Monad.MonadError`, 
by making use of the transformation functions such as `trans`, `trans2`, etc.
Admittedly it is tedious though, you can deeply understand monad-transformation mechanism instead.
-}
{-# LANGUAGE MultiParamTypeClasses, 
             FlexibleInstances #-}
module DeepControl.Monad.RWS (
    MonadReader(..), MonadWriter(..), MonadState(..),

    -- * Level-0
    RWS(..), rws, evalRWS, execRWS, mapRWS, withRWS,
    -- * Level-1
    RWST(..), rwsT, evalRWST, execRWST, mapRWST, withRWST, liftCatch,
    -- * Level-2
    RWST2(..), rwsT2, evalRWST2, execRWST2, mapRWST2, withRWST2, 
    -- * Level-3
    RWST3(..), rwsT3, evalRWST3, execRWST3, mapRWST3, withRWST3, 

    ) where 

import DeepControl.Applicative
import DeepControl.Monad
import DeepControl.MonadTrans

import Control.Monad.Reader (MonadReader(..))
import Control.Monad.Writer (MonadWriter(..))
import Control.Monad.State (MonadState(..))
import Control.Monad.Signatures
import Data.Monoid

----------------------------------------------------------------------
-- Level-0

newtype RWS r w s a = RWS { runRWS :: r -> s -> (a, s, w) }

instance Functor (RWS r w s) where
    fmap f m = RWS $ \r s ->
        (\(a, s', w) -> (f a, s', w)) $ runRWS m r s
instance (Monoid w) => Applicative (RWS r w s) where
    pure a = RWS $ \_ s -> (a, s, mempty)
    (<*>)  = ap
instance (Monoid w) => Monad (RWS r w s) where
    return = pure
    m >>= k = RWS $ \r s -> 
        runRWS m r s >- \(a, s', w) ->
        runRWS (k a) r s' >- \(b, s'',w') ->
        (b, s'', w <> w')
instance (Monoid w) => MonadReader r (RWS r w s) where
    ask       = RWS $ \r s -> (r, s, mempty)
    local f m = RWS $ \r s -> runRWS m (f r) s
instance (Monoid w) => MonadWriter w (RWS r w s) where
    writer (a, w) = RWS $ \_ s -> (a, s, w)
    tell w        = RWS $ \_ s -> ((),s,w)
    listen m      = RWS $ \r s -> 
        runRWS m r s >- \(a, s', w) ->
        ((a, w), s', w)
    pass m        = RWS $ \r s ->
        runRWS m r s >- \((a, f), s', w) ->
        (a, s', f w)
instance (Monoid w) => MonadState s (RWS r w s) where
    get   = RWS $ \_ s -> (s, s, mempty)
    put s = RWS $ \_ _ -> ((), s, mempty)

rws :: (r -> s -> (a, s, w)) -> RWS r w s a
rws = RWS
evalRWS :: RWS r w s a -> r -> s -> (a, w)
evalRWS m r s =
    runRWS m r s >- \(a, _, w) ->
    (a, w)
execRWS :: RWS r w s a -> r -> s -> (s, w)
execRWS m r s =
    runRWS m r s >- \(_, s', w) ->
    (s', w)
mapRWS :: ((a, s, w) -> (b, s, w')) -> RWS r w s a -> RWS r w' s b
mapRWS f m = RWS $ \r s -> f (runRWS m r s)
withRWS :: (r' -> s -> (r, s)) -> RWS r w s a -> RWS r' w s a
withRWS f m = RWS $ \r s -> uncurry (runRWS m) (f r s)

----------------------------------------------------------------------
-- Level-1

newtype RWST r w s m a = RWST { runRWST :: r -> s -> m (a, s, w) }

instance (Functor m) => Functor (RWST r w s m) where
    fmap f m = RWST $ \r s ->
        (\(a, s', w) -> (f a, s', w)) |$> runRWST m r s
instance (Monoid w, Monad m) => Applicative (RWST r w s m) where
    pure a = RWST $ \_ s -> (*:) (a, s, mempty)
    (<*>)  = ap
instance (Monoid w, Monad m) => Monad (RWST r w s m) where
    return = pure
    m >>= k = RWST $ \r s -> 
        runRWST m r s >>= \(a, s', w) ->
        runRWST (k a) r s' >>= \(b, s'',w') ->
        (*:) (b, s'', w <> w')
instance (Monoid w, Monad m) => MonadReader r (RWST r w s m) where
    ask       = RWST $ \r s -> (*:) (r, s, mempty)
    local f m = RWST $ \r s -> runRWST m (f r) s
instance (Monoid w, Monad m) => MonadWriter w (RWST r w s m) where
    writer (a, w) = RWST $ \_ s -> (*:) (a, s, w)
    tell w        = RWST $ \_ s -> (*:) ((), s, w)
    listen m      = RWST $ \r s -> 
        runRWST m r s >>= \(a, s', w) ->
        (*:) ((a, w), s', w)
    pass m        = RWST $ \r s ->
        runRWST m r s >>= \((a, f), s', w) ->
        (*:) (a, s', f w)
instance (Monoid w, Monad m) => MonadState s (RWST r w s m) where
    get   = RWST $ \_ s -> (*:) (s, s, mempty)
    put s = RWST $ \_ _ -> (*:) ((), s, mempty)

instance (Monoid w) => MonadTrans (RWST r w s) where
    trans m = RWST $ \r s -> 
        m >>= \a ->
        (*:) (a, s, mempty)
instance (Monoid w, MonadIO m, Monad m) => MonadIO (RWST r w s m) where
    liftIO = trans . liftIO

rwsT :: (Monad m) => (r -> s -> (a, s, w)) -> RWST r w s m a
rwsT = RWST . (--*)
evalRWST :: (Monad m) => RWST r w s m a -> r -> s -> m (a, w)
evalRWST m r s =
    runRWST m r s >>= \(a, _, w) ->
    (*:) (a, w)
execRWST :: (Monad m) => RWST r w s m a -> r -> s -> m (s, w)
execRWST m r s =
    runRWST m r s >>= \(_, s', w) ->
    (*:) (s', w)

mapRWST :: (m (a, s, w) -> n (b, s, w')) -> RWST r w s m a -> RWST r w' s n b
mapRWST f m = RWST $ \r s -> f (runRWST m r s)
withRWST :: (r' -> s -> (r, s)) -> RWST r w s m a -> RWST r' w s m a
withRWST f m = RWST $ \r s -> uncurry (runRWST m) (f r s)

liftCatch :: Catch e m (a,s,w) -> Catch e (RWST r w s m) a
liftCatch catchE m h =
    RWST $ \r s -> runRWST m r s `catchE` \e -> runRWST (h e) r s

----------------------------------------------------------------------
-- Level-2

newtype RWST2 r w s m1 m2 a = RWST2 { runRWST2 :: r -> s -> m1 (m2 (a, s, w)) }

instance (Functor m1, Functor m2) => Functor (RWST2 r w s m1 m2) where
    fmap f m = RWST2 $ \r s ->
        (\(a, s', w) -> (f a, s', w)) |$>> runRWST2 m r s
instance (Monoid w, Monad m1, Monad2 m2) => Applicative (RWST2 r w s m1 m2) where
    pure a = RWST2 $ \_ s -> (**:) (a, s, mempty)
    (<*>)  = ap
instance (Monoid w, Monad m1, Monad2 m2) => Monad (RWST2 r w s m1 m2) where
    return = pure
    m >>= k = RWST2 $ \r s -> 
        runRWST2 m r s >>== \(a, s', w) ->
        runRWST2 (k a) r s' >>== \(b, s'',w') ->
        (**:) (b, s'', w <> w')
instance (Monoid w, Monad m1, Monad2 m2) => MonadReader r (RWST2 r w s m1 m2) where
    ask       = RWST2 $ \r s -> (**:) (r, s, mempty)
    local f m = RWST2 $ \r s -> runRWST2 m (f r) s
instance (Monoid w, Monad m1, Monad2 m2) => MonadWriter w (RWST2 r w s m1 m2) where
    writer (a, w) = RWST2 $ \_ s -> (**:) (a, s, w)
    tell w        = RWST2 $ \_ s -> (**:) ((),s,w)
    listen m      = RWST2 $ \r s -> 
        runRWST2 m r s >>== \(a, s', w) ->
        (**:) ((a, w), s', w)
    pass m        = RWST2 $ \r s ->
        runRWST2 m r s >>== \((a, f), s', w) ->
        (**:) (a, s', f w)
instance (Monoid w, Monad m1, Monad2 m2) => MonadState s (RWST2 r w s m1 m2) where
    get   = RWST2 $ \_ s -> (**:) (s, s, mempty)
    put s = RWST2 $ \_ _ -> (**:) ((), s, mempty)

instance (Monoid w) => MonadTrans2 (RWST2 r w s) where
    trans2 m = RWST2 $ \r s -> 
        m >>== \a ->
        (**:) (a, s, mempty)
instance (Monoid w, MonadIO m1, Monad m1, Monad2 m2) => MonadIO (RWST2 r w s m1 m2) where
    liftIO = trans2 . (-*) . liftIO

rwsT2 :: (Monad m1, Monad2 m2) => (r -> s -> (a, s, w)) -> RWST2 r w s m1 m2 a
rwsT2 = RWST2 . ((**:)|$>>)
evalRWST2 :: (Monad m1, Monad2 m2) => RWST2 r w s m1 m2 a -> r -> s -> m1 (m2 (a, w))
evalRWST2 m r s =
    runRWST2 m r s >>== \(a, _, w) ->
    (**:) (a, w)
execRWST2 :: (Monad m1, Monad2 m2) => RWST2 r w s m1 m2 a -> r -> s -> m1 (m2 (s, w))
execRWST2 m r s =
    runRWST2 m r s >>== \(_, s', w) ->
    (**:) (s', w)

mapRWST2 :: (m1 (m2 (a, s, w)) -> n1 (n2 (b, s, w'))) -> RWST2 r w s m1 m2 a -> RWST2 r w' s n1 n2 b
mapRWST2 f m = RWST2 $ \r s -> f (runRWST2 m r s)
withRWST2 :: (r' -> s -> (r, s)) -> RWST2 r w s m1 m2 a -> RWST2 r' w s m1 m2 a
withRWST2 f m = RWST2 $ \r s -> uncurry (runRWST2 m) (f r s)

----------------------------------------------------------------------
-- Level-3

newtype RWST3 r w s m1 m2 m3 a = RWST3 { runRWST3 :: r -> s -> m1 (m2 (m3 (a, s, w))) }

instance (Functor m1, Functor m2, Functor m3) => Functor (RWST3 r w s m1 m2 m3) where
    fmap f m = RWST3 $ \r s ->
        (\(a, s', w) -> (f a, s', w)) |$>>> runRWST3 m r s
instance (Monoid w, Monad m1, Monad2 m2, Monad3 m3) => Applicative (RWST3 r w s m1 m2 m3) where
    pure a = RWST3 $ \_ s -> (***:) (a, s, mempty)
    (<*>)  = ap
instance (Monoid w, Monad m1, Monad2 m2, Monad3 m3) => Monad (RWST3 r w s m1 m2 m3) where
    return = pure
    m >>= k = RWST3 $ \r s -> 
        runRWST3 m r s >>>== \(a, s', w) ->
        runRWST3 (k a) r s' >>>== \(b, s'',w') ->
        (***:) (b, s'', w <> w')
instance (Monoid w, Monad m1, Monad2 m2, Monad3 m3) => MonadReader r (RWST3 r w s m1 m2 m3) where
    ask       = RWST3 $ \r s -> (***:) (r, s, mempty)
    local f m = RWST3 $ \r s -> runRWST3 m (f r) s
instance (Monoid w, Monad m1, Monad2 m2, Monad3 m3) => MonadWriter w (RWST3 r w s m1 m2 m3) where
    writer (a, w) = RWST3 $ \_ s -> (***:) (a, s, w)
    tell w        = RWST3 $ \_ s -> (***:) ((), s, w)
    listen m      = RWST3 $ \r s -> 
        runRWST3 m r s >>>== \(a, s', w) ->
        (***:) ((a, w), s', w)
    pass m        = RWST3 $ \r s ->
        runRWST3 m r s >>>== \((a, f), s', w) ->
        (***:) (a, s', f w)
instance (Monoid w, Monad m1, Monad2 m2, Monad3 m3) => MonadState s (RWST3 r w s m1 m2 m3) where
    get   = RWST3 $ \_ s -> (***:) (s, s, mempty)
    put s = RWST3 $ \_ _ -> (***:) ((), s, mempty)

instance (Monoid w) => MonadTrans3 (RWST3 r w s) where
    trans3 m = RWST3 $ \r s -> 
        m >>>== \a ->
        (***:) (a, s, mempty)
instance (Monoid w, MonadIO m1, Monad m1, Monad2 m2, Monad3 m3) => MonadIO (RWST3 r w s m1 m2 m3) where
    liftIO = trans3 . (-**) . liftIO

rwsT3 :: (Monad m1, Monad2 m2, Monad3 m3) => (r -> s -> (a, s, w)) -> RWST3 r w s m1 m2 m3 a
rwsT3 = RWST3 . ((***:)|$>>)
evalRWST3 :: (Monad m1, Monad2 m2, Monad3 m3) => RWST3 r w s m1 m2 m3 a -> r -> s -> m1 (m2 (m3 (a, w)))
evalRWST3 m r s =
    runRWST3 m r s >>>== \(a, _, w) ->
    (***:) (a, w)
execRWST3 :: (Monad m1, Monad2 m2, Monad3 m3) => RWST3 r w s m1 m2 m3 a -> r -> s -> m1 (m2 (m3 (s, w)))
execRWST3 m r s =
    runRWST3 m r s >>>== \(_, s', w) ->
    (***:) (s', w)

mapRWST3 :: (m1 (m2 (m3 (a, s, w))) -> n1 (n2 (n3 (b, s, w')))) -> RWST3 r w s m1 m2 m3 a -> RWST3 r w' s n1 n2 n3 b
mapRWST3 f m = RWST3 $ \r s -> f (runRWST3 m r s)
withRWST3 :: (r' -> s -> (r, s)) -> RWST3 r w s m1 m2 m3 a -> RWST3 r' w s m1 m2 m3 a
withRWST3 f m = RWST3 $ \r s -> uncurry (runRWST3 m) (f r s)

