{-|
Module      : DeepControl.Monad.Except
Description : 
Copyright   : (C) 2013 Ross Paterson,
              (C) 2015 KONISHI Yohsuke 
License     : BSD-style (see the file LICENSE)
Maintainer  : ocean0yohsuke@gmail.com
Stability   : experimental
Portability : ---

This module is just a concise mimic for Except Monad in mtl(monad-transformer-library).
The qualifier "concise" means that this module doesn't make no attempt to transform functions of any kind of Monad automatically.
So when making some new data type of ExceptT, you have to manually define involved Monad instances, 
for example `DeepControl.Monad.MonadReader`, `DeepControl.Monad.MonadWriter` or `DeepControl.Monad.MonadState`, 
by making use of the transformation functions such as `trans`, `trans2`, etc.
Admittedly it is tedious though, you can deeply understand monad-transformation mechanism instead.
-}
{-# LANGUAGE 
        DeriveFunctor,
        GeneralizedNewtypeDeriving,
        FlexibleInstances,
        FunctionalDependencies
 #-}
module DeepControl.Monad.Except (
    -- * Classes
    Error(..),
    MonadError(..),

    -- * Level-0
    Except(..), mapExcept, withExcept, 
    -- * Level-1
    ExceptT(..), mapExceptT, withExceptT, 

    -- * Examples
    -- $examples
    ) where 

import DeepControl.Applicative
import DeepControl.Monad
import DeepControl.MonadTrans
import DeepControl.Commutative

import Control.Monad.Except (MonadError(..))
import Control.Monad.Signatures
import Data.Monoid

class Error a where
    noMsg  :: a
    noMsg    = strMsg ""
    strMsg :: String -> a
    strMsg _ = noMsg

----------------------------------------------------------------------
-- Level-0

newtype Except e a = Except { runExcept :: Either e a }
  deriving (Show, Functor, Applicative, Monad, MonadError e) 

instance Commutative (Except e) where
    commute (Except x) = Except |$> commute x

mapExcept :: (Either e a -> Either e' b) -> Except e a -> Except e' b
mapExcept f = Except . f . runExcept

withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept f = mapExcept $ either (Left . f) Right

----------------------------------------------------------------------
-- Level-1

newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }

instance (Functor m) => Functor (ExceptT e m) where
    fmap f = \(ExceptT mv) -> ExceptT $ f |$>> mv
instance (Monad m) => Applicative (ExceptT e m) where
    pure a = ExceptT $ (*:) (Right a)
    (<*>)  = ap
instance (Monad m) => Monad (ExceptT e m) where
    return = pure
    m >>= f = ExceptT $ runExceptT m >>== \v -> runExceptT (f v)

instance (Monad m, Error e) => MonadError e (ExceptT e m) where
    throwError = ExceptT . (*:) . Left
    m `catchError` h = ExceptT $ do
        a <- runExceptT m
        case a of
            Left  l -> runExceptT (h l)
            Right r -> (*:) (Right r)

mapExceptT :: (m (Either e a) -> n (Either e' b)) -> ExceptT e m a -> ExceptT e' n b
mapExceptT f = ExceptT . f . runExceptT

withExceptT :: Functor m => (e -> e') -> ExceptT e m a -> ExceptT e' m a
withExceptT f = mapExceptT $ fmap $ either (Left . f) Right

{- $examples


-}

