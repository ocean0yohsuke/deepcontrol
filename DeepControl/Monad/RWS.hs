{-# LANGUAGE MultiParamTypeClasses, 
             FlexibleInstances #-}
module DeepControl.Monad.RWS (
    MonadReader(..), MonadWriter(..), MonadState(..),

    RWS(..), rws, evalRWS, execRWS, 

    ) where 

import DeepControl.Applicative
import DeepControl.Monad

import Control.Monad.Reader (MonadReader(..))
import Control.Monad.Writer (MonadWriter(..))
import Control.Monad.State (MonadState(..))

----------------------------------------------------------------------
-- RWS

newtype RWS r w s a = RWS { runRWS :: r -> s -> (a, s, w) }

instance Functor (RWS r w s) where
    fmap f m = RWS $ \r s ->
        (\(a, s', w) -> (f a, s', w)) $ runRWS m r s
instance (Monoid w) => Applicative (RWS r w s) where
    pure a = RWS $ \_ s -> (a, s, mempty)
    (<*>)  = ap
instance (Monoid w) => Monad (RWS r w s) where
    return  = (*:)
    m >>= k = RWS $ \r s -> 
        runRWS m r s >- \(a, s', w) ->
        runRWS (k a) r s' >- \(b, s'',w') ->
        (b, s'', w `mappend` w')
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


