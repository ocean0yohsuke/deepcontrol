-----------------------------------------------------------------------------
-- | Deepened the usual Control.Monad.Signatures module.
-- Module      :  DeepControl.Monad.Signatures
-- Copyright   :  (c) Ross Paterson 2012,
--                (c) 2015 KONISHI Yohsuke
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  KONISHI Yohsuke
-- Stability   :  experimental
-- Portability :  portable
--
-- Signatures for monad operations that require specialized lifting.
-- Each signature has a uniformity property that the lifting should satisfy.
-----------------------------------------------------------------------------

module DeepControl.Monad.Signatures (
    -- * Level-1
    module Control.Monad.Signatures,

    -- * Level-2
    CallCC2, Catch2, Listen2, Pass2,

    -- * Level-3
    CallCC3, Catch3, Listen3, Pass3,

    -- * Level-4
    CallCC4, Catch4, Listen4, Pass4,

    -- * Level-5
    CallCC5, Catch5, Listen5, Pass5,

  ) where

import Control.Monad.Signatures

-------------------------------------------------------------------------
-- Level-2

type CallCC2 m1 m2 a b = ((a -> m1 (m2 b)) -> m1 (m2 a)) -> m1 (m2 a)

type Catch2 e m1 m2 a = m1 (m2 a) -> (e -> m1 (m2 a)) -> m1 (m2 a)

type Listen2 w m1 m2 a = m1 (m2 a) -> m1 (m2 (a, w))

type Pass2 w m1 m2 a = m1 (m2 (a, w -> w)) -> m1 (m2 a)

-------------------------------------------------------------------------
-- Level-3

type CallCC3 m1 m2 m3 a b = ((a -> m1 (m2 (m3 b))) -> m1 (m2 (m3 a))) -> m1 (m2 (m3 a))

type Catch3 e m1 m2 m3 a = m1 (m2 (m3 a)) -> (e -> m1 (m2 (m3 a))) -> m1 (m2 (m3 a))

type Listen3 w m1 m2 m3 a = m1 (m2 (m3 a)) -> m1 (m2 (m3 (a, w)))

type Pass3 w m1 m2 m3 a = m1 (m2 (m3 (a, w -> w))) -> m1 (m2 (m3 a))

-------------------------------------------------------------------------
-- Level-4

type CallCC4 m1 m2 m3 m4 a b = ((a -> m1 (m2 (m3 (m4 b)))) -> m1 (m2 (m3 (m4 a)))) -> m1 (m2 (m3 (m4 a)))

type Catch4 e m1 m2 m3 m4 a = m1 (m2 (m3 (m4 a))) -> (e -> m1 (m2 (m3 (m4 a)))) -> m1 (m2 (m3 (m4 a)))

type Listen4 w m1 m2 m3 m4 a = m1 (m2 (m3 (m4 a))) -> m1 (m2 (m3 (m4 (a, w))))

type Pass4 w m1 m2 m3 m4 a = m1 (m2 (m3 (m4 (a, w -> w)))) -> m1 (m2 (m3 (m4 a)))

-------------------------------------------------------------------------
-- Level-5

type CallCC5 m1 m2 m3 m4 m5 a b = ((a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 a))))) -> m1 (m2 (m3 (m4 (m5 a))))

type Catch5 e m1 m2 m3 m4 m5 a = m1 (m2 (m3 (m4 (m5 a)))) -> (e -> m1 (m2 (m3 (m4 (m5 a))))) -> m1 (m2 (m3 (m4 (m5 a))))

type Listen5 w m1 m2 m3 m4 m5 a = m1 (m2 (m3 (m4 (m5 a)))) -> m1 (m2 (m3 (m4 (m5 (a, w)))))

type Pass5 w m1 m2 m3 m4 m5 a = m1 (m2 (m3 (m4 (m5 (a, w -> w))))) -> m1 (m2 (m3 (m4 (m5 a))))

