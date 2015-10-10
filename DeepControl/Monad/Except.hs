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

{- 
TODO: 例えば newdata SomeType = ReaderT2 r IO (Except e) a とした場合、 MonadError e SomeType　のインスタンスが作れない（catchError がどうしても作れない）
　　　なので Except の Monadx は作ってもあまり意味がない。

>newtype Eval a = Eval { unEval :: RWST2 Env [String] Int IO (Except ExpError) a }
>  deriving (Functor, Applicative, Monad, MonadIO)
>
>class (Monad2 m2) => MonadError2 e m2 | m2 -> e where
>  throwError2 :: (Monad m1) => e -> m1 (m2 a)
>  catchError2 :: (Monad m1) => m1 (m2 a) -> (e -> m1 (m2 a)) -> m1 (m2 a)
>
>instance MonadError2 e Except where
>  throwError2     = (*:) . throwError
>  catchError2 x h = 
>        x >>= \x' -> 
>        case x' of
>          Right v -> (*:) x'
>          Left e  -> catchError e h
>
>instance MonadError ExpError Eval where
>    throwError     = Eval . trans2 . throwError2
>    catchError x f = 作れない

instance Monad2 (Except e) where
    m >>== f = (Except |$>) $ (runExcept |$> m) >>== runExcept |$>> f
instance Monad3 (Except e) where
    m >>>== f = (Except |$>>) $ (runExcept |$>> m) >>>== runExcept |$>>> f
instance Monad4 (Except e) where
    m >>>>== f = (Except |$>>>) $ (runExcept |$>>> m) >>>>== runExcept |$>>>> f
instance Monad5 (Except e) where
    m >>>>>== f = (Except |$>>>>) $ (runExcept |$>>>> m) >>>>>== runExcept |$>>>>> f
-}

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

instance MonadTrans (ExceptT e) where
    trans = ExceptT . (-*)
instance (MonadIO m) => MonadIO (ExceptT e m) where
    liftIO = trans . liftIO

mapExceptT :: (m (Either e a) -> n (Either e' b)) -> ExceptT e m a -> ExceptT e' n b
mapExceptT f = ExceptT . f . runExceptT

withExceptT :: Functor m => (e -> e') -> ExceptT e m a -> ExceptT e' m a
withExceptT f = mapExceptT $ fmap $ either (Left . f) Right


