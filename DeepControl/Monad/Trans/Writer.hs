{-|
Module      : DeepControl.Monad.Trans.Writer
Description : Extension for mtl's Contrl.Monad.Writer.
Copyright   : (c) Andy Gill 2001,
              (c) Oregon Graduate Institute of Science and Technology, 2001,
              (c) 2015 KONISHI Yohsuke,
License     : BSD-style (see the file LICENSE)
Maintainer  : ocean0yohsuke@gmail.com
Stability   : experimental
Portability : ---

This module extended Writer monad of mtl(monad-transformer-library).
-}
module DeepControl.Monad.Trans.Writer (
    module Control.Monad.Writer,

    -- * Level-2
    listen2, pass2,
    -- * Level-3
    listen3, pass3,
    -- * Level-4
    listen4, pass4,
    -- * Level-5
    listen5, pass5,

    ) where 

import DeepControl.Applicative

import Control.Monad.Writer
import Data.Monoid

----------------------------------------------------------------------
-- Level-2

listen2 :: (MonadWriter w m2, Applicative m1) => m1 (m2 a) -> m1 (m2 (a, w))
listen2 m = listen |$> m
pass2 :: (MonadWriter w m2, Applicative m1) => m1 (m2 (a, w -> w)) -> m1 (m2 a)
pass2 m = pass |$> m

----------------------------------------------------------------------
-- Level-3

listen3 :: (MonadWriter w m3, Applicative m1, Applicative m2) => m1 (m2 (m3 a)) -> m1 (m2 (m3 (a, w)))
listen3 m = listen2 |$> m
pass3 :: (MonadWriter w m3, Applicative m1, Applicative m2) => m1 (m2 (m3 (a, w -> w))) -> m1 (m2 (m3 a))
pass3 m = pass2 |$> m

----------------------------------------------------------------------
-- Level-4

listen4 :: (MonadWriter w m4, Applicative m1, Applicative m2, Applicative m3) => m1 (m2 (m3 (m4 a))) -> m1 (m2 (m3 (m4 (a, w))))
listen4 m = listen3 |$> m
pass4 :: (MonadWriter w m4, Applicative m1, Applicative m2, Applicative m3) => m1 (m2 (m3 (m4 (a, w -> w)))) -> m1 (m2 (m3 (m4 a)))
pass4 m = pass3 |$> m

----------------------------------------------------------------------
-- Level-5

listen5 :: (MonadWriter w m5, Applicative m1, Applicative m2, Applicative m3, Applicative m4) => m1 (m2 (m3 (m4 (m5 a)))) -> m1 (m2 (m3 (m4 (m5 (a, w)))))
listen5 m = listen4 |$> m
pass5 :: (MonadWriter w m5, Applicative m1, Applicative m2, Applicative m3, Applicative m4) => m1 (m2 (m3 (m4 (m5 (a, w -> w))))) -> m1 (m2 (m3 (m4 (m5 a))))
pass5 m = pass4 |$> m

