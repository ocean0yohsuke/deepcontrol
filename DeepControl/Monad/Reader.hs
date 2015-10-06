{-# LANGUAGE MultiParamTypeClasses, 
             FlexibleInstances #-}
module DeepControl.Monad.Reader (
    MonadReader(..),
    asks,

    Reader(..),

    ) where 

import DeepControl.Applicative
import DeepControl.Monad

import Control.Monad.Reader (MonadReader(..))

asks :: MonadReader r m => (r -> a) -> m a
asks = reader

----------------------------------------------------------------------
-- Reader

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
    fmap f v = Reader $ \r -> 
        f $ runReader v r 
instance Applicative (Reader r) where
    pure a = Reader $ \_ -> a
    (<*>) = ap
instance Monad (Reader r) where
    return = (*:)
    mv >>= f = mv >- \(Reader v) -> Reader $ \r ->
        v r >- \a -> 
        runReader (f a) r

{-
instance Monad2 (Reader r) where
    -- TODO: Reader を Commutative にできていないため、これは無理
    mmv >>== f = mmv >>= \(Reader v) -> commute $ Reader $ \r ->
        v r >- \a ->
        runReader |$> f a |* r
-}  

instance MonadReader r (Reader r) where
    ask       = Reader id
    local f m = Reader $ runReader m . f


