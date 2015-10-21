{-|
Module      : DeepControl.Monad.Morph
Description : Extension for mmorph's Contrl.Monad.Morph.
Copyright   : 2013 Gabriel Gonzalez,
              (C) 2015 KONISHI Yohsuke 
License     : BSD-style (see the LICENSE file in the distribution)
Maintainer  : ocean0yohsuke@gmail.com
Stability   : experimental
Portability : ---

This module enables you to program in Monad-Morphic style for more __deeper__ level than the usual @Control.Monad.Morph@ module expresses.
You would realize exactly what __/more deeper level/__ means by reading the example codes, which are attached on the page bottom.
Note: many instances for Level-4 and Level-5 haven't been written yet.
-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module DeepControl.Monad.Morph (
    module Control.Monad.Morph,

    -- * Level-1
    (|>|), (|<|), 

    -- * Level-2
    (|>>|), (|<<|),

    -- * Level-3
    (|>>>|), (|<<<|),

    -- * Level-4
    (|>>>>|), (|<<<<|),

    -- * Level-5
    (|>>>>>|), (|<<<<<|),

    -- * Level-2 example
    -- $Example_Level2

    ) where 

import DeepControl.Applicative

import Control.Monad.Morph

-------------------------------------------------------------------------------
-- Level-1 functions

infixl 2  |>|
-- | Alias for @'hoist'@.
(|>|) :: (Monad m, MFunctor t) => (forall a . m a -> n a) -> t m b -> t n b
(|>|) = hoist

infixr 2  |<|
-- | Equivalent to (|>|) with the arguments flipped.
(|<|) :: (Monad m, MFunctor t) => t m b -> (forall a . m a -> n a) -> t n b
(|<|) l r = hoist r l

-------------------------------------------------------------------------------
-- Level-2 functions

infixl 2  |>>|
(|>>|) :: (Monad m, Monad (t2 m), MFunctor t1, MFunctor t2) => 
          (forall a . m a -> n a) -> t1 (t2 m) b -> t1 (t2 n) b
(|>>|) f g = (f |>|) |>| g

infixr 2  |<<|
(|<<|) :: (Monad m, Monad (t2 m), MFunctor t1, MFunctor t2) => 
           t1 (t2 m) b -> (forall a . m a -> n a) -> t1 (t2 n) b
(|<<|) f g = (g |>|) |>| f

-------------------------------------------------------------------------------
-- Level-3 functions

infixl 2  |>>>|
(|>>>|) :: (Monad m, Monad (t3 m), Monad (t2 (t3 m)), MFunctor t1, MFunctor t2, MFunctor t3) => 
           (forall a . m a -> n a) -> t1 (t2 (t3 m)) b -> t1 (t2 (t3 n)) b
(|>>>|) f g = (f |>|) |>>| g

infixr 2  |<<<|
(|<<<|) :: (Monad m, Monad (t3 m), Monad (t2 (t3 m)), MFunctor t1, MFunctor t2, MFunctor t3) => 
           t1 (t2 (t3 m)) b -> (forall a . m a -> n a) -> t1 (t2 (t3 n)) b
(|<<<|) f g = (g |>|) |>>| f

-------------------------------------------------------------------------------
-- Level-4 functions

infixl 2  |>>>>|
(|>>>>|) :: (Monad m, Monad (t4 m), Monad (t3 (t4 m)), Monad (t2 (t3 (t4 m))), MFunctor t1, MFunctor t2, MFunctor t3, MFunctor t4) => 
            (forall a . m a -> n a) -> t1 (t2 (t3 (t4 m))) b -> t1 (t2 (t3 (t4 n))) b
(|>>>>|) f g = (f |>|) |>>>| g

infixr 2  |<<<<|
(|<<<<|) :: (Monad m, Monad (t4 m), Monad (t3 (t4 m)), Monad (t2 (t3 (t4 m))), MFunctor t1, MFunctor t2, MFunctor t3, MFunctor t4) => 
            t1 (t2 (t3 (t4 m))) b -> (forall a . m a -> n a) -> t1 (t2 (t3 (t4 n))) b
(|<<<<|) f g = (g |>|) |>>>| f

-------------------------------------------------------------------------------
-- Level-5 functions

infixl 2  |>>>>>|
(|>>>>>|) :: (Monad m, Monad (t5 m), Monad (t4 (t5 m)), Monad (t3 (t4 (t5 m))), Monad (t2 (t3 (t4 (t5 m)))), MFunctor t1, MFunctor t2, MFunctor t3, MFunctor t4, MFunctor t5) => 
             (forall a . m a -> n a) -> t1 (t2 (t3 (t4 (t5 m)))) b -> t1 (t2 (t3 (t4 (t5 n)))) b
(|>>>>>|) f g = (f |>|) |>>>>| g

infixr 2  |<<<<<|
(|<<<<<|) :: (Monad m, Monad (t5 m), Monad (t4 (t5 m)), Monad (t3 (t4 (t5 m))), Monad (t2 (t3 (t4 (t5 m)))), MFunctor t1, MFunctor t2, MFunctor t3, MFunctor t4, MFunctor t5) => 
             t1 (t2 (t3 (t4 (t5 m)))) b -> (forall a . m a -> n a) -> t1 (t2 (t3 (t4 (t5 n)))) b
(|<<<<<|) f g = (g |>|) |>>>>| f

-------------------------------------------------------------------------------
-- TODO

{-
infixr 2  |>>=
class (MFunctor t2, MonadTrans t2) => MMonad2 t2 where
    (|>>=) :: (Monad n, Monad m, MMonad t1) => t1 (t2 m) b -> (forall a . m a -> t1 (t2 n) a) -> t1 (t2 n) b

instance MMonad2 I.IdentityT where
    m |>>= f = (I.runIdentityT |>| m) |>= f

infixr 2  >||>
(>||>) :: (Monad m2, Monad m3, MMonad t1, MMonad2 t2) => 
          (forall a. m1 a -> t1 (t2 m2) a) -> (forall b. m2 b -> t1 (t2 m3) b) -> m1 c -> t1 (t2 m3) c
(f >||> g) m = f m |>>= g

-}

----------------------------------------------------------------------
-- Examples

{- $Example_Level2
Here is a monad-morph example, a level-2 monad-morph.

>import DeepControl.Monad.Morph
>import DeepControl.Monad.Trans.State
>import DeepControl.Monad.Trans.Writer
>
>-- i.e. :: StateT Int Identity ()
>tick    :: State Int ()
>tick = modify (+1)
>
>tock                        ::                   StateT Int IO ()
>tock = do
>    generalize |>| tick     :: (Monad      m) => StateT Int m  ()
>    lift $ putStrLn "Tock!" :: (MonadTrans t) => t          IO ()
>
>-- λ> runStateT tock 0
>-- Tock!
>-- ((),1)
>
>-- i.e. :: StateT Int (WriterT [Int] Identity) ()
>save    :: StateT Int (Writer  [Int]) ()
>save = do
>    n <- get
>    lift $ tell [n]
>
>program ::                   StateT Int (WriterT [Int] IO) ()
>program = replicateM_ 4 $ do
>    lift |>| tock
>        :: (MonadTrans t) => StateT Int (t             IO) ()
>    generalize |>>| save
>        :: (Monad      m) => StateT Int (WriterT [Int] m ) ()
>
>-- λ> execWriterT (runStateT program 0)
>-- Tock!
>-- Tock!
>-- Tock!
>-- Tock!
>-- [1,2,3,4]
-}
