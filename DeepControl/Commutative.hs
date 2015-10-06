{-|
Module      : DeepControl.Commutative
Description : Commutative Functor, Applicative, Monad.
Copyright   : KONISHI Yohuske 2015,
              Conor McBride and Ross Paterson 2005
License     : BSD-style (see the LICENSE file in the distribution)
Maintainer  : ocean0yohsuke@gmail.com
Stability   : experimental
Portability : ---

This module is made of @'Data.Traversable'@, distilling most function names polluted with action kind of concepts into crystalized(static) ones.
Another reason I put this module is for the case if GHC would parse @((->) r)@ as a data constructor someday.

-}
module DeepControl.Commutative (
    -- * The 'Commutative' class
    Commutative(..),
    -- * Utility functions
    commuteMap,
    commuteFor,
    -- * General definitions for superclass methods
    fmapDefault,
    foldMapDefault,
    ) where 

import DeepControl.Applicative

------------------------------------------------------------------------------
-- Commutative

-- | 
-- 
class (Functor c) => Commutative c where
  -- | This method is the same for @'Data.Traversable.sequenceA'@ just except the name.
  --   The only difference is the name "commute", that is to say from which no action kind of concepts smell.
  commute :: Applicative f => c (f a) -> f (c a)

-- | Do @fmap f@ then commute, the same for @'Data.Traversable.traverse'@.
commuteMap :: (Applicative f, Commutative c) => (a -> f b) -> c a -> f (c b)
commuteMap f = commute . (f |$>)
-- | The auguments-flipped function for @'commuteMap'@, the same for @'Data.Traversable.for'@.
commuteFor :: (Applicative f, Commutative c) => c a -> (a -> f b) -> f (c b)
commuteFor = flip commuteMap

instance Commutative Maybe where
    commute (Just fa) = Just |$> fa
    commute Nothing = (*:) Nothing

instance Commutative [] where
    commute = foldr (\x acc -> x <$|(:)|*> acc) ((*:) [])
  
instance Commutative (Either a) where
    commute (Right x) = Right |$> x
    commute (Left x) = (*:) $ Left x

instance Commutative ((,) a) where
    commute (x, y) = x <|(,)|$> y

instance Commutative (Const m) where
    commute (Const m) = (*:) $ Const m

{-
instance Commutative ((->) r) where
    -- TODO: If GHC could parse this expression, maybe I could write up DeepControl.Monad.
    commute ((r->) mv) = (r->) |$> mv
-}

-- | This function may be used as a value for `fmap` in a `Functor`
--   instance, provided that 'commute' is defined. (Using
--   `fmapDefault` with a `Commutative` instance will result in infinite recursion.)
fmapDefault :: Commutative t => (a -> b) -> t a -> t b
fmapDefault f = getId . commuteMap (Id . f)

-- | This function may be used as a value for `Data.Foldable.foldMap`
--   in a `Foldable` instance.
foldMapDefault :: (Commutative t, Monoid m) => (a -> m) -> t a -> m
foldMapDefault f = getConst . commuteMap (Const . f)

-- local instances
newtype Id a = Id { getId :: a }
instance Functor Id where
    fmap f (Id x) = Id (f x)
instance Applicative Id where
    pure = Id
    Id f <*> Id x = Id (f x)



