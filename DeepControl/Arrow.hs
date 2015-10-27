{-|
Module      : DeepControl.Arrow
Description : Deepened the usual Control.Arrow module.
Copyright   : (c) Ross Paterson 2002,
              (c) 2015 KONISHI Yohsuke 
License     : BSD-style (see the LICENSE file in the distribution)
Maintainer  : ocean0yohsuke@gmail.com
Stability   : experimental
Portability : ---

-}
module DeepControl.Arrow (
    module Control.Arrow,
    
    Kleisli2(..),
    Kleisli3(..),
    Kleisli4(..),
    Kleisli5(..),

    ) where 

import DeepControl.Applicative
import DeepControl.Traversable
import DeepControl.Monad
import Control.Arrow
import Prelude hiding (id, (.))
import Control.Category

----------------------------------------------------------------------
-- Kleisli2

newtype Kleisli2 m1 m2 a b = Kleisli2 { runKleisli2 :: a -> m1 (m2 b) }

instance (Monad m1, Monad m2, Traversable m2) => Category (Kleisli2 m1 m2) where
    id = Kleisli2 $ (.**) 
    (Kleisli2 g) . (Kleisli2 f) = Kleisli2 $ f >==> g

instance (Monad m1, Monad m2, Traversable m2) => Arrow (Kleisli2 m1 m2) where
    arr f = Kleisli2 $ (.**) . f
    first (Kleisli2 f) = Kleisli2 $ \ ~(b,d) -> f b >>== \c -> (.**) (c,d)
    second (Kleisli2 f) = Kleisli2 $ \ ~(d,b) -> f b >>== \c -> (.**) (d,c)

----------------------------------------------------------------------
-- Kleisli3

newtype Kleisli3 m1 m2 m3 a b = Kleisli3 { runKleisli3 :: a -> m1 (m2 (m3 b)) }

instance (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3) => Category (Kleisli3 m1 m2 m3) where
    id = Kleisli3 $ (.***) 
    (Kleisli3 g) . (Kleisli3 f) = Kleisli3 $ f >===> g

instance (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3) => Arrow (Kleisli3 m1 m2 m3) where
    arr f = Kleisli3 $ (.***) . f
    first (Kleisli3 f) = Kleisli3 $ \ ~(b,d) -> f b >>>== \c -> (.***) (c,d)
    second (Kleisli3 f) = Kleisli3 $ \ ~(d,b) -> f b >>>== \c -> (.***) (d,c)

----------------------------------------------------------------------
-- Kleisli4

newtype Kleisli4 m1 m2 m3 m4 a b = Kleisli4 { runKleisli4 :: a -> m1 (m2 (m3 (m4 b))) }

instance (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4) => Category (Kleisli4 m1 m2 m3 m4) where
    id = Kleisli4 $ (.****) 
    (Kleisli4 g) . (Kleisli4 f) = Kleisli4 $ f >====> g

instance (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4) => Arrow (Kleisli4 m1 m2 m3 m4) where
    arr f = Kleisli4 $ (.****) . f
    first (Kleisli4 f) = Kleisli4 $ \ ~(b,d) -> f b >>>>== \c -> (.****) (c,d)
    second (Kleisli4 f) = Kleisli4 $ \ ~(d,b) -> f b >>>>== \c -> (.****) (d,c)

----------------------------------------------------------------------
-- Kleisli5

newtype Kleisli5 m1 m2 m3 m4 m5 a b = Kleisli5 { runKleisli5 :: a -> m1 (m2 (m3 (m4 (m5 b)))) }

instance (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => Category (Kleisli5 m1 m2 m3 m4 m5) where
    id = Kleisli5 $ (.*****)
    (Kleisli5 g) . (Kleisli5 f) = Kleisli5 $ f >=====> g

instance (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => Arrow (Kleisli5 m1 m2 m3 m4 m5) where
    arr f = Kleisli5 $ (.*****) . f
    first (Kleisli5 f) = Kleisli5 $ \ ~(b,d) -> f b >>>>>== \c -> (.*****) (c,d)
    second (Kleisli5 f) = Kleisli5 $ \ ~(d,b) -> f b >>>>>== \c -> (.*****) (d,c)

