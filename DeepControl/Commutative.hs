{-|
Module      : DeepControl.Commutative
Description : Commutative Functor.
Copyright   : Conor McBride and Ross Paterson 2005,
              (C) 2015 KONISHI Yohsuke 
License     : BSD-style (see the LICENSE file in the distribution)
Maintainer  : ocean0yohsuke@gmail.com
Stability   : experimental
Portability : ---

This module is made of @'Data.Traversable'@, distilling most function names polluted with action kind of concepts into crystalized(static) ones.
Another reason I put this module is for the case if GHC would parse @((->) r)@ as a data constructor someday.

-}
module DeepControl.Commutative (
    -- * Level-1
    -- ** The 'Commutative' class
    Commutative(..),
    -- ** Utility functions
    cmap,
    cfor,
    -- ** General definitions for superclass methods
    fmapDefault,
    foldMapDefault,

    -- * Level-2
    sink2, float2,

    -- * Level-3
    sink3, float3,

    -- * Level-4
    sink4, float4,

    -- * Level-5
    sink5, float5,

    ) where 

import DeepControl.Applicative

import Data.Monoid

------------------------------------------------------------------------------
-- Level-1

-- | 
-- 
class (Applicative c) => Commutative c where
  -- | This method is equivalent for @'Data.Traversable.sequenceA'@ just except the name.
  --   The only difference is the name "commute", that is to say from which no action kind of concepts smell.
  commute :: Applicative f => c (f a) -> f (c a)

-- | Do @fmap f@ then commute, equivalent for @'Data.Traversable.traverse'@.
cmap :: (Applicative f, Commutative c) => (a -> f b) -> c a -> f (c b)
cmap f = commute . (f |$>)
-- | The auguments-flipped function for @'cmap'@, equivalent for @'Data.Traversable.for'@.
cfor :: (Applicative f, Commutative c) => c a -> (a -> f b) -> f (c b)
cfor = flip cmap

instance Commutative Maybe where
    commute (Just fa) = Just |$> fa
    commute Nothing   = (*:) Nothing

instance Commutative [] where
    commute = foldr (\x acc -> x <$|(:)|*> acc) ((*:) [])
  
instance Commutative (Either a) where
    commute (Right x) = Right |$> x
    commute (Left x)  = (*:) $ Left x

{-
instance Commutative ((->) r) where
    -- TODO: If GHC could parse this expression, maybe I could write up DeepControl.Monad.
    commute ((r->) mv) = (r->) |$> mv
-}

-- | This function may be used as a value for `fmap` in a `Functor`
--   instance, provided that 'commute' is defined. (Using
--   `fmapDefault` with a `Commutative` instance will result in infinite recursion.)
fmapDefault :: Commutative t => (a -> b) -> t a -> t b
fmapDefault f = getId . cmap (Id . f)

-- | This function may be used as a value for `Data.Foldable.foldMap`
--   in a `Foldable` instance.
foldMapDefault :: (Commutative t, Monoid m) => (a -> m) -> t a -> m
foldMapDefault f = getConst . cmap (Const . f)

-- local instances
newtype Id a = Id { getId :: a }
instance Functor Id where
    fmap f (Id x) = Id (f x)
instance Applicative Id where
    pure = Id
    Id f <*> Id x = Id (f x)

------------------------------------------------------------------------------
-- Level-2

sink2 :: (Commutative m1, Commutative m2, Applicative m3) => 
         m1 (m2 (m3 a)) -> m2 (m3 (m1 a))
sink2 = (commute|$>) . commute

float2 :: (Applicative m1, Commutative m2, Commutative m3) => 
          m2 (m3 (m1 a)) -> m1 (m2 (m3 a))
float2 = commute . (commute|$>)

------------------------------------------------------------------------------
-- Level-3

sink3 :: (Commutative m1, Commutative m2, Commutative m3, Applicative m4) => 
         m1 (m2 (m3 (m4 a))) -> m2 (m3 (m4 (m1 a)))
sink3 = (sink2|$>) . commute

float3 :: (Applicative m1, Commutative m2, Commutative m3, Commutative m4) => 
          m2 (m3 (m4 (m1 a))) -> m1 (m2 (m3 (m4 a)))
float3 = commute . (float2|$>)

------------------------------------------------------------------------------
-- Level-4

sink4 :: (Commutative m1, Commutative m2, Commutative m3, Commutative m4, Applicative m5) => 
         m1 (m2 (m3 (m4 (m5 a)))) -> m2 (m3 (m4 (m5 (m1 a))))
sink4 = (sink3|$>) . commute

float4 :: (Applicative m1, Commutative m2, Commutative m3, Commutative m4, Commutative m5) => 
          m2 (m3 (m4 (m5 (m1 a)))) -> m1 (m2 (m3 (m4 (m5 a))))
float4 = commute . (float3|$>)

------------------------------------------------------------------------------
-- Level-5

sink5 :: (Commutative m1, Commutative m2, Commutative m3, Commutative m4, Commutative m5, Applicative m6) => 
         m1 (m2 (m3 (m4 (m5 (m6 a))))) -> m2 (m3 (m4 (m5 (m6 (m1 a)))))
sink5 = (sink4|$>) . commute

float5 :: (Applicative m1, Commutative m2, Commutative m3, Commutative m4, Commutative m5, Commutative m6) => 
          m2 (m3 (m4 (m5 (m6 (m1 a))))) -> m1 (m2 (m3 (m4 (m5 (m6 a)))))
float5 = commute . (float4|$>)


