{-|
Module      : DeepControl.Traversable
Description : Deepened the usual Data.Traversable module.
Copyright   : Conor McBride and Ross Paterson 2005,
              (c) 2015 KONISHI Yohsuke
License     : BSD-style (see the LICENSE file in the distribution)
Maintainer  : ocean0yohsuke@gmail.com
Stability   : experimental
Portability : ---

This module is deepened of @'Data.Traversable'@.
-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module DeepControl.Traversable (
    -- * Utility functions
    -- ** Level-1
    sink, sinkmap,
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

------------------------------------------------------------------------------
-- Level-1

-- | Alias to @'Data.Traversable.sequenceA'@.
-- 
-- >>> sink $ Just [1]
-- [Just 1]
--
-- >>> sink $ Right (Just 1)
-- Just (Right 1)
--
-- >>> sink [[1,2],[3,4]]
-- [[1,3],[1,4],[2,3],[2,4]]
-- >>> sink . sink $ [[1,2],[3,4]]
-- [[1,1,2,2],[1,1,2,4],[1,1,3,2],[1,1,3,4],[1,4,2,2],[1,4,2,4],[1,4,3,2],[1,4,3,4],[3,1,2,2],[3,1,2,4],[3,1,3,2],[3,1,3,4],[3,4,2,2],[3,4,2,4],[3,4,3,2],[3,4,3,4]]
--
sink :: (Traversable t, Applicative f) => t (f a) -> f (t a)
sink = sequenceA

{-
instance Traversable ((->) r) where
    -- TODO: If GHC could parse this expression, maybe I could write up DeepControl.Monad.
    sequence ((r->) mv) = (r->) |$> mv
-}

-- | Alias to @'Data.Traversable.traversable'@.
sinkmap :: (Applicative f, Traversable c) => (a -> f b) -> c a -> f (c b)
sinkmap f = sink . (f |$>)

------------------------------------------------------------------------------
-- Level-2

-- | Definition: @ sink2 = (sink|$>) . sink @
--
-- >>> sink2 $ Just [[1]]
-- [[Just 1]]
--
-- >>> sink2 $ Right (Just [1])
-- Just [Right 1]
--
sink2 :: (Traversable m1, Applicative m2, Applicative m3) =>
         m1 (m2 (m3 a)) -> m2 (m3 (m1 a))
sink2 = (sink|$>) . sink

-- | Definition: @ float2 = sink . (sink|$>) @
--
-- >>> float2 $ [[Just 1]]
-- Just [[1]]
--
-- >>> float2 $ Just [Right 1]
-- Right (Just [1])
--
float2 :: (Applicative m1, Traversable m2, Traversable m3) =>
          m2 (m3 (m1 a)) -> m1 (m2 (m3 a))
float2 = sink . (sink|$>)

------------------------------------------------------------------------------
-- Level-3

-- | Definition: @ sink3 = (sink2|$>) . sink @
--
-- >>> sink3 $ Just [[[1]]]
-- [[[Just 1]]]
--
-- >>> sink3 $ Right [Just [1]]
-- [Just [Right 1]]
--
sink3 :: (Traversable m1, Applicative m2, Applicative m3, Applicative m4) =>
         m1 (m2 (m3 (m4 a))) -> m2 (m3 (m4 (m1 a)))
sink3 = (sink2|$>) . sink

-- | Definition: @ float3 = sink . (float2|$>) @
--
-- >>> float3 $ [[[Just 1]]]
-- Just [[[1]]]
--
-- >>> float3 $ [Just [Right 1]]
-- Right [Just [1]]
--
float3 :: (Applicative m1, Traversable m2, Traversable m3, Traversable m4) =>
          m2 (m3 (m4 (m1 a))) -> m1 (m2 (m3 (m4 a)))
float3 = sink . (float2|$>)

------------------------------------------------------------------------------
-- Level-4

sink4 :: (Traversable m1, Applicative m2, Applicative m3, Applicative m4, Applicative m5) =>
         m1 (m2 (m3 (m4 (m5 a)))) -> m2 (m3 (m4 (m5 (m1 a))))
sink4 = (sink3|$>) . sink

float4 :: (Applicative m1, Traversable m2, Traversable m3, Traversable m4, Traversable m5) =>
          m2 (m3 (m4 (m5 (m1 a)))) -> m1 (m2 (m3 (m4 (m5 a))))
float4 = sink . (float3|$>)

------------------------------------------------------------------------------
-- Level-5

sink5 :: (Traversable m1, Applicative m2, Applicative m3, Applicative m4, Applicative m5, Applicative m6) =>
         m1 (m2 (m3 (m4 (m5 (m6 a))))) -> m2 (m3 (m4 (m5 (m6 (m1 a)))))
sink5 = (sink4|$>) . sink

float5 :: (Applicative m1, Traversable m2, Traversable m3, Traversable m4, Traversable m5, Traversable m6) =>
          m2 (m3 (m4 (m5 (m6 (m1 a))))) -> m1 (m2 (m3 (m4 (m5 (m6 a)))))
float5 = sink . (float4|$>)
