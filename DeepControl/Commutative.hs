{-|
Module      : DeepControl.Commutative
Description : ---
Copyright   : Conor McBride and Ross Paterson 2005,
              (c) 2015 KONISHI Yohsuke 
License     : BSD-style (see the LICENSE file in the distribution)
Maintainer  : ocean0yohsuke@gmail.com
Stability   : experimental
Portability : ---

This module is made of @'Data.Traversable'@, distilling most function names polluted with action kind of concepts into crystalized(static) ones.
-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module DeepControl.Commutative (
    -- * The 'Commutative' class
    Commutative(..),
    -- * Utility functions
    cmap,
    cfor,
    -- * General definitions for superclass methods
    fmapDefault,
    foldMapDefault,

    -- * Utility functions 2
    -- ** Level-2
    sink2, float2,
    -- ** Level-3
    sink3, float3,
    -- ** Level-4
    sink4, float4,
    -- ** Level-5
    sink5, float5,

    ) where 

import DeepControl.Applicative
import Data.Monoid

import Control.Monad.Identity (Identity(..))
import Control.Monad.Except (Except, ExceptT(..), runExcept)
import Control.Monad.Writer (Writer, WriterT(..), runWriter)

------------------------------------------------------------------------------
-- Level-1

-- | [], Maybe, Either, Except and Writer are all commutative each other.
--   So these monads can be deepened to Monad2, Monad3, Monad4 and Monad5.
-- 
class (Functor c) => Commutative c where
  -- | This method is equivalent for @'Data.Traversable.sequenceA'@ except the name.
  --   The only difference is the name "commute", that is to say from which no action kind of concepts smell.
  --
  -- >>> commute $ Just [1]
  -- [Just 1]
  -- >>> commute $ [Just 1]
  -- Just [1]
  --
  -- >>> commute $ Right (Just 1)
  -- Just (Right 1)
  -- >>> commute $ Just (Right 1)
  -- Right (Just 1)
  --
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
  
instance (Monoid w) => Commutative (Writer w) where
    commute x = 
        let (a, b) = runWriter x
        in  (WriterT . Identity) |$> (a <$|(,)|* b)

instance Commutative (Either a) where
    commute (Right x) = Right |$> x
    commute (Left x)  = (*:) $ Left x
instance Commutative (Except e) where
    commute x = ExceptT . Identity |$> commute (runExcept x)

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

-- | sink2 = (commute|$>) . commute
--
-- >>> sink2 $ Right (Just [1])
-- Just [Right 1]
--
sink2 :: (Commutative m1, Applicative m2, Applicative m3) => 
         m1 (m2 (m3 a)) -> m2 (m3 (m1 a))
sink2 = (commute|$>) . commute

-- | float2 = commute . (commute|$>)
--
-- >>> float2 $ Just [Right 1]
-- Right (Just [1])
--
float2 :: (Applicative m1, Commutative m2, Commutative m3) => 
          m2 (m3 (m1 a)) -> m1 (m2 (m3 a))
float2 = commute . (commute|$>)

------------------------------------------------------------------------------
-- Level-3

-- | sink3 = (sink2|$>) . commute
--
-- >>> sink3 $ Right [Just [1]]
-- [Just [Right 1]]
--
sink3 :: (Commutative m1, Applicative m2, Applicative m3, Applicative m4) => 
         m1 (m2 (m3 (m4 a))) -> m2 (m3 (m4 (m1 a)))
sink3 = (sink2|$>) . commute

-- | float3 = commute . (float2|$>)
--
-- >>> float3 $ [Just [Right 1]]
-- Right [Just [1]]
--
float3 :: (Applicative m1, Commutative m2, Commutative m3, Commutative m4) => 
          m2 (m3 (m4 (m1 a))) -> m1 (m2 (m3 (m4 a)))
float3 = commute . (float2|$>)

------------------------------------------------------------------------------
-- Level-4

sink4 :: (Commutative m1, Applicative m2, Applicative m3, Applicative m4, Applicative m5) => 
         m1 (m2 (m3 (m4 (m5 a)))) -> m2 (m3 (m4 (m5 (m1 a))))
sink4 = (sink3|$>) . commute

float4 :: (Applicative m1, Commutative m2, Commutative m3, Commutative m4, Commutative m5) => 
          m2 (m3 (m4 (m5 (m1 a)))) -> m1 (m2 (m3 (m4 (m5 a))))
float4 = commute . (float3|$>)

------------------------------------------------------------------------------
-- Level-5

sink5 :: (Commutative m1, Applicative m2, Applicative m3, Applicative m4, Applicative m5, Applicative m6) => 
         m1 (m2 (m3 (m4 (m5 (m6 a))))) -> m2 (m3 (m4 (m5 (m6 (m1 a)))))
sink5 = (sink4|$>) . commute

float5 :: (Applicative m1, Commutative m2, Commutative m3, Commutative m4, Commutative m5, Commutative m6) => 
          m2 (m3 (m4 (m5 (m6 (m1 a))))) -> m1 (m2 (m3 (m4 (m5 (m6 a)))))
float5 = commute . (float4|$>)


