{-|
Module      : DeepControl.Monad
Description : Deepend the usual Control.Monad module.
Copyright   : (c) The University of Glasgow 2001,
              (c) 2015 KONISHI Yohsuke 
License     : BSD-style (see the LICENSE file in the distribution)
Maintainer  : ocean0yohsuke@gmail.com
Stability   : experimental
Portability : ---

This module enables you to program in Monad for __much deeper level__ than the usual 'Control.Monad' module expresses.
You would soon realize exactly what __/much deeper level/__ means by reading the example codes attached on some functions below.

Note: 

    * This module never makes mtl(monad-transformer-library) unnessasary at all. 
      The range of monad, for which this module is effective,  is confined to the range that the instances of @'Traversable'@ are defined.
      This means disappointingly that this module is not effective for not-Traversable monads such as 'Reader' and 'State'.
    * In my opinion this imperfection is hard-wired with the ability of GHC on lambda expression; GHC is unable to parse "@(r->)@" or "@((->) r)@" yet.
      Therefore some fundamental expressions such as "@(r->)|$>@" or "@fmap (r->)@" are useless, though such calculations might theoretically or mathematically be impossible.

-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module DeepControl.Monad (
    module Control.Monad,

    -- * Level-0
    -- ** bind 
    (-<), (>-), 
    -- ** fish 
    (>->), (<-<),

    -- * Level-2
    -- ** bind
    (>>==), 
    -- ** sequence
    (>>~), 
    -- ** cover-bind
    (>-=), (->=), 
    -- ** cover-sequence
    (>-~), (->~),
    -- ** fish 
    (>>=>), 

    -- * Level-3
    -- ** bind
    (>>>=), 
    -- ** sequence
    (>>>~),
    -- ** cover-bind 
    (>>-=), (->>=), (>->=) ,(>--=),(->-=), (-->=), 
    -- ** cover-sequence 
    (>--~), (->-~), (-->~), (>>-~), (->>~), (>->~), 
    -- ** fish 
    (>>>=>),

    -- * Level-4
    -- ** bind
    (>>>>=), 
    -- ** sequence
    (>>>>~), 
    -- ** cover-bind 
    (--->=), (-->-=), (->--=), (>---=),
    (-->>=), (->->=), (>-->=), (>->-=), (->>-=), (>>--=),
    (->>>=), (>->>=), (>>->=), (>>>-=),
    -- ** cover-sequence 
    (--->~), (-->-~), (->--~), (>---~),
    (-->>~), (->->~), (>-->~), (>->-~), (->>-~), (>>--~),
    (->>>~), (>->>~), (>>->~), (>>>-~),
    -- ** fish 
    (>>>>=>), 

    -- * Level-5
    -- ** bind
    (>>>>>=), 
    -- ** sequence
    (>>>>>~), 
    -- ** cover-bind 
    (---->=), (--->-=), (-->--=), (->---=), (>----=),
    (--->>=), (-->->=), (->-->=), (>--->=), (>-->-=), (->->-=), (-->>-=), (->>--=), (>->--=), (>>---=),
    (-->>>=), (->->>=), (>-->>=), (>->->=), (->>->=), (>>-->=), (>>->-=), (>->>-=), (->>>-=), (>>>--=),
    (->>>>=), (>->>>=), (>>->>=), (>>>->=), (>>>>-=),
    -- ** cover-sequence 
    (---->~), (--->-~), (-->--~), (->---~), (>----~),
    (--->>~), (-->->~), (->-->~), (>--->~), (>-->-~), (->->-~), (-->>-~), (->>--~), (>->--~), (>>---~),
    (-->>>~), (->->>~), (>-->>~), (>->->~), (->>->~), (>>-->~), (>>->-~), (>->>-~), (->>>-~), (>>>--~),
    (->>>>~), (>->>>~), (>>->>~), (>>>->~), (>>>>-~),
    -- ** fish 
    (>>>>>=>), 

    ) where 

import DeepControl.Applicative
import DeepControl.Traversable

import Control.Monad

-------------------------------------------------------------------------------
-- Level-0 functions

infixl 1  -<, >-
-- | Anologous to @'$'@, but (-<) is left associative.
--
-- >>> Just -< 3
-- Just 3
(-<) :: (a -> b) -> a -> b
(-<) = ($)
-- | The auguments-flipped function of @'-<'@.
--
-- >>> 3 >- Just
-- Just 3
--
-- >>> :{
-- let plus :: Int -> Int -> Int
--     plus x y = 
--         x >- \a ->  -- (>-) is the level-0 bind function, analogous to (>>==)
--         y >- \b ->
--         a + b
-- in plus 3 4
-- :}
-- 7
--
(>-) :: a -> (a -> b) -> b
(>-) = flip (-<)

infixr 1  <-<, >->
-- | Anologous to @'.'@, but the infix preference is low.
--
-- >>> ((3+) <-< (2*) <-< (1+)) -< 1
-- 7
(<-<) :: (b -> c) -> (a -> b) -> a -> c
(<-<) = (.)
-- | The auguments-flipped function of @'<-<'@. 
--
-- >>> 1 >- ((+1) >-> (*2) >-> (+3))
-- 7
(>->) :: (a -> b) -> (b -> c) -> a -> c
(>->) = flip (<-<)

-------------------------------------------------------------------------------
-- Level-1 functions

-------------------------------------------------------------------------------
-- Level-2 functions

infixr 1  >>=>
infixr 1  >>==, >>~
infixr 1  ->=, >-=
infixr 1  ->~, >-~

-- | The level-2 bind function, analogous to @'>>='@.
-- 
-- >>> :{
--  -- List-List monad
--  [["a","b"]] >>== \x ->   -- (>>==) is the level-2 bind function, analogous to (>>==)
--  [[0],[1,2]] >>== \y -> 
--  (.**) (x ++ show y)
-- :}
-- [["a0","b0"],["a0","b1","b2"],["a1","a2","b0"],["a1","a2","b1","b2"]]
--
-- >>> :{
--  let lengthM :: [Int] -> Maybe Int      -- (->)-Maybe monad
--      lengthM [] = Nothing
--      lengthM xs = Just (length xs) 
--      averageM :: [Int] -> Maybe Double  -- (->)-Maybe monad
--      averageM = 
--          (-*) sum >>== \s ->
--          lengthM >>== \l ->
--          (.**) (fromIntegral s / fromIntegral l)
--  in [averageM [10, 25, 70], averageM []]
-- :}
-- [Just 35.0,Nothing]
--
(>>==) :: (Monad m1, Monad m2, Traversable m2) => m1 (m2 a) -> (a -> m1 (m2 b)) -> m1 (m2 b)
m >>== f = m >>= \x -> join |$> (sink $ f |$> x)

-- | The level-2 fish function, analogous to @'>=>'@.
(>>=>) :: (Monad m1, Monad m2, Traversable m2) => (a -> m1 (m2 b)) -> (b -> m1 (m2 c)) -> a -> m1 (m2 c)
f >>=> g = \x -> f x >>== g
-- | The level-2 sequence function, analogous to @'>>'@.
--
--   Definition: @ m >>~ f = m >>= \\_ -> f @
(>>~) :: (Monad m1, Monad m2, Traversable m2) => m1 (m2 a) -> m1 (m2 b) -> m1 (m2 b)
m >>~ f = m >>== \_ -> f
-- | A level-2 cover-bind function, analogous to @'>>='@.
--
--   Definition: @ m >-= f = (-*) m >>= f @
(>-=) :: (Monad m1, Monad m2, Traversable m2) => m1 a -> (a -> m1 (m2 b)) -> m1 (m2 b)
m >-= f = (-*) m >>== f
-- | A level-2 cover-bind function, analogous to @'>>='@.
--
--   Definition: @ m ->= f = (.*) m >>= f @
(->=) :: (Monad m1, Monad m2, Traversable m2) => m2 a -> (a -> m1 (m2 b)) -> m1 (m2 b)
m ->= f = (.*) m >>== f
-- | A level-2 cover-sequence function, analogous to @'>>'@.
--
--   Definition: @ m >-~ f = (-*) m >>~ f @
(>-~) :: (Monad m1, Monad m2, Traversable m2) => m1 a -> m1 (m2 b) -> m1 (m2 b)
m >-~ f = (-*) m >>~ f
-- | A level-2 cover-sequence function, analogous to @'>>'@.
--
--   Definition: @ m ->~ f = (.*) m >>~ f @
(->~) :: (Monad m1, Monad m2, Traversable m2) => m2 a -> m1 (m2 b) -> m1 (m2 b)
m ->~ f = (.*) m >>~ f

-------------------------------------------------------------------------------
-- Level-3 functions

infixr 1  >>>=>
infixr 1  >>>=, >>>~
infixr 1  >--=, ->-=, -->=, >>-=, >->=, ->>=
infixr 1  >--~, ->-~, -->~, >>-~, >->~, ->>~

-- | The level-3 bind function, analogous to @'>>='@.
-- 
-- >>> :{
--  -- IO-List-List monad
--  (.*) [["a","b"]] >>>= \x ->   -- (>>>=) is the level-3 bind function, analogous to (>>==)
--  (.*) [[0],[1,2]] >>>= \y ->
--  (-**) (print (x,y)) >>>~      -- (>>>~) is the level-3 sequence function, analogous to (>>)
--  (.***) (x ++ show y)
-- :}
-- ("a",0)
-- ("a",1)
-- ("a",2)
-- ("b",0)
-- ("b",1)
-- ("b",2)
-- [["a0","b0"],["a0","b1","b2"],["a1","a2","b0"],["a1","a2","b1","b2"]]
--
-- Any messy code like this can be neatly rewritten to a code below by grace of some cover functions.
--
-- >>> :{
--  -- IO-List-List monad
--  [["a","b"]] ->>= \x ->   -- (->>=) is a level-3 cover-bind function, analogous to (>>==)
--  [[0],[1,2]] ->>= \y ->
--  print (x,y) >--~         -- (>--~) is a level-3 cover-sequence function, analogous to (>>)
--  (.***) (x ++ show y)
-- :}
-- ("a",0)
-- ("a",1)
-- ("a",2)
-- ("b",0)
-- ("b",1)
-- ("b",2)
-- [["a0","b0"],["a0","b1","b2"],["a1","a2","b0"],["a1","a2","b1","b2"]]
--
(>>>=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3) => m1 (m2 (m3 a)) -> (a -> m1 (m2 (m3 b))) -> m1 (m2 (m3 b))
m >>>= f = m >>== \x -> join |$>> (sink2 $ f |$> x)

(>>>=>) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3) => (a -> m1 (m2 (m3 b))) -> (b -> m1 (m2 (m3 c))) -> a -> m1 (m2 (m3 c))
f >>>=> g = \x -> f x >>>= g
(>>>~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3) => m1 (m2 (m3 a)) -> m1 (m2 (m3 b)) -> m1 (m2 (m3 b))
m >>>~ k = m >>>= \_ -> k

(>--=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3) => m1 a -> (a -> m1 (m2 (m3 b))) -> m1 (m2 (m3 b))
m >--= k = (-**) m >>>= k
(->-=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3) => m2 a -> (a -> m1 (m2 (m3 b))) -> m1 (m2 (m3 b))
m ->-= k = (*-*) m >>>= k
(-->=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3) => m3 a -> (a -> m1 (m2 (m3 b))) -> m1 (m2 (m3 b))
m -->= k = (.**) m >>>= k
(>>-=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3) => m1 (m2 a) -> (a -> m1 (m2 (m3 b))) -> m1 (m2 (m3 b))
m >>-= k = (--*) m >>>= k
(->>=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3) => m2 (m3 a) -> (a -> m1 (m2 (m3 b))) -> m1 (m2 (m3 b))
m ->>= k = (.*) m >>>= k
(>->=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3) => m1 (m3 a) -> (a -> m1 (m2 (m3 b))) -> m1 (m2 (m3 b))
m >->= k = (-*) m >>>= k
(-->~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3) => m3 a -> m1 (m2 (m3 b)) -> m1 (m2 (m3 b))
m -->~ k = (.**) m >>>~ k
(->-~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3) => m2 a -> m1 (m2 (m3 b)) -> m1 (m2 (m3 b))
m ->-~ k = (*-*) m >>>~ k
(>--~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3) => m1 a -> m1 (m2 (m3 b)) -> m1 (m2 (m3 b))
m >--~ k = (-**) m >>>~ k
(->>~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3) => m2 (m3 a) -> m1 (m2 (m3 b)) -> m1 (m2 (m3 b))
m ->>~ k = (.*) m >>>~ k
(>->~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3) => m1 (m3 a) -> m1 (m2 (m3 b)) -> m1 (m2 (m3 b))
m >->~ k = (-*) m >>>~ k
(>>-~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3) => m1 (m2 a) -> m1 (m2 (m3 b)) -> m1 (m2 (m3 b))
m >>-~ k = (--*) m >>>~ k

-------------------------------------------------------------------------------
-- Level-4 functions

infixr 1  >>>>=>
infixr 1  >>>>=, >>>>~
infixr 1  --->=, -->-=, ->--=, >---=
infixr 1  -->>=, ->->=, >-->=, >->-=, ->>-=, >>--=
infixr 1  ->>>=, >->>=, >>->=, >>>-=
infixr 1  --->~, -->-~, ->--~, >---~
infixr 1  -->>~, ->->~, >-->~, >->-~, ->>-~, >>--~
infixr 1  ->>>~, >->>~, >>->~, >>>-~

(>>>>=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4) => m1 (m2 (m3 (m4 a))) -> (a -> m1 (m2 (m3 (m4 b)))) -> m1 (m2 (m3 (m4 b)))
m >>>>= f = m >>>= \x -> join |$>>> (sink3 $ f |$> x)

(>>>>=>) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4) => (a -> m1 (m2 (m3 (m4 b)))) -> (b -> m1 (m2 (m3 (m4 c)))) -> a -> m1 (m2 (m3 (m4 c)))
f >>>>=> g = \x -> f x >>>>= g
(>>>>~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4) => m1 (m2 (m3 (m4 a))) -> m1 (m2 (m3 (m4 b))) -> m1 (m2 (m3 (m4 b)))
m >>>>~ k = m >>>>= \_ -> k

(--->=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4) => m4 a -> (a -> m1 (m2 (m3 (m4 b)))) -> m1 (m2 (m3 (m4 b)))
m --->= k = (.***) m >>>>= k 
(-->-=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4) => m3 a -> (a -> m1 (m2 (m3 (m4 b)))) -> m1 (m2 (m3 (m4 b)))
m -->-= k = (**-*) m >>>>= k 
(->--=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4) => m2 a -> (a -> m1 (m2 (m3 (m4 b)))) -> m1 (m2 (m3 (m4 b)))
m ->--= k = (*-**) m >>>>= k 
(>---=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4) => m1 a -> (a -> m1 (m2 (m3 (m4 b)))) -> m1 (m2 (m3 (m4 b)))
m >---= k = (-***) m >>>>= k

(-->>=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4) => m3 (m4 a) -> (a -> m1 (m2 (m3 (m4 b)))) -> m1 (m2 (m3 (m4 b)))
m -->>= k = (.**) m >>>>= k 
(->->=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4) => m2 (m4 a) -> (a -> m1 (m2 (m3 (m4 b)))) -> m1 (m2 (m3 (m4 b)))
m ->->= k = (*-*) m >>>>= k 
(>-->=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4) => m1 (m4 a) -> (a -> m1 (m2 (m3 (m4 b)))) -> m1 (m2 (m3 (m4 b)))
m >-->= k = (-**) m >>>>= k 
(>->-=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4) => m1 (m3 a) -> (a -> m1 (m2 (m3 (m4 b)))) -> m1 (m2 (m3 (m4 b)))
m >->-= k = (-*-*) m >>>>= k 
(->>-=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4) => m2 (m3 a) -> (a -> m1 (m2 (m3 (m4 b)))) -> m1 (m2 (m3 (m4 b)))
m ->>-= k = (*--*) m >>>>= k 
(>>--=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4) => m1 (m2 a) -> (a -> m1 (m2 (m3 (m4 b)))) -> m1 (m2 (m3 (m4 b)))
m >>--= k = (--**) m >>>>= k 

(->>>=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4) => m2 (m3 (m4 a)) -> (a -> m1 (m2 (m3 (m4 b)))) -> m1 (m2 (m3 (m4 b)))
m ->>>= k = (.*) m >>>>= k 
(>->>=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4) => m1 (m3 (m4 a)) -> (a -> m1 (m2 (m3 (m4 b)))) -> m1 (m2 (m3 (m4 b)))
m >->>= k = (-*) m >>>>= k 
(>>->=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4) => m1 (m2 (m4 a)) -> (a -> m1 (m2 (m3 (m4 b)))) -> m1 (m2 (m3 (m4 b)))
m >>->= k = (--*) m >>>>= k 
(>>>-=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4) => m1 (m2 (m3 a)) -> (a -> m1 (m2 (m3 (m4 b)))) -> m1 (m2 (m3 (m4 b)))
m >>>-= k = (---*) m >>>>= k 

(--->~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4) => m4 a -> m1 (m2 (m3 (m4 b))) -> m1 (m2 (m3 (m4 b)))
m --->~ k = (.***) m >>>>~ k
(-->-~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4) => m3 a -> m1 (m2 (m3 (m4 b))) -> m1 (m2 (m3 (m4 b)))
m -->-~ k = (**-*) m >>>>~ k
(->--~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4) => m2 a -> m1 (m2 (m3 (m4 b))) -> m1 (m2 (m3 (m4 b)))
m ->--~ k = (*-**) m >>>>~ k
(>---~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4) => m1 a -> m1 (m2 (m3 (m4 b))) -> m1 (m2 (m3 (m4 b)))
m >---~ k = (-***) m >>>>~ k

(-->>~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4) => m3 (m4 a) -> m1 (m2 (m3 (m4 b))) -> m1 (m2 (m3 (m4 b)))
m -->>~ k = (.**) m >>>>~ k
(->->~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4) => m2 (m4 a) -> m1 (m2 (m3 (m4 b))) -> m1 (m2 (m3 (m4 b)))
m ->->~ k = (*-*) m >>>>~ k
(>-->~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4) => m1 (m4 a) -> m1 (m2 (m3 (m4 b))) -> m1 (m2 (m3 (m4 b)))
m >-->~ k = (-**) m >>>>~ k
(>->-~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4) => m1 (m3 a) -> m1 (m2 (m3 (m4 b))) -> m1 (m2 (m3 (m4 b)))
m >->-~ k = (-*-*) m >>>>~ k
(->>-~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4) => m2 (m3 a) -> m1 (m2 (m3 (m4 b))) -> m1 (m2 (m3 (m4 b)))
m ->>-~ k = (*--*) m >>>>~ k
(>>--~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4) => m1 (m2 a) -> m1 (m2 (m3 (m4 b))) -> m1 (m2 (m3 (m4 b)))
m >>--~ k = (--**) m >>>>~ k

(->>>~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4) => m2 (m3 (m4 a)) -> m1 (m2 (m3 (m4 b))) -> m1 (m2 (m3 (m4 b)))
m ->>>~ k = (.*) m >>>>~ k
(>->>~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4) => m1 (m3 (m4 a)) -> m1 (m2 (m3 (m4 b))) -> m1 (m2 (m3 (m4 b)))
m >->>~ k = (-*) m >>>>~ k
(>>->~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4) => m1 (m2 (m4 a)) -> m1 (m2 (m3 (m4 b))) -> m1 (m2 (m3 (m4 b)))
m >>->~ k = (--*) m >>>>~ k
(>>>-~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4) => m1 (m2 (m3 a)) -> m1 (m2 (m3 (m4 b))) -> m1 (m2 (m3 (m4 b)))
m >>>-~ k = (---*) m >>>>~ k

-------------------------------------------------------------------------------
-- Level-5 functions

infixr 1  >>>>>=>
infixr 1  >>>>>~, >>>>>=
infixr 1  ---->=, --->-=, -->--=, ->---=, >----=
infixr 1  --->>=, -->->=, ->-->=, >--->=, >-->-=, ->->-=, -->>-=, ->>--=, >->--=, >>---=
infixr 1  -->>>=, ->->>=, >-->>=, >->->=, ->>->=, >>-->=, >>->-=, >->>-=, ->>>-=, >>>--=
infixr 1  ->>>>=, >->>>=, >>->>=, >>>->=, >>>>-=

infixr 1  ---->~, --->-~, -->--~, ->---~, >----~
infixr 1  --->>~, -->->~, ->-->~, >--->~, >-->-~, ->->-~, -->>-~, ->>--~, >->--~, >>---~
infixr 1  -->>>~, ->->>~, >-->>~, >->->~, ->>->~, >>-->~, >>->-~, >->>-~, ->>>-~, >>>--~
infixr 1  ->>>>~, >->>>~, >>->>~, >>>->~, >>>>-~

(>>>>>=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m1 (m2 (m3 (m4 (m5 a)))) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m >>>>>= f = m >>>>= \x -> join |$>>>> (sink4 $ f |$> x)

(>>>>>=>) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => (a -> m1 (m2 (m3 (m4 (m5 b))))) -> (b -> m1 (m2 (m3 (m4 (m5 c))))) -> a -> m1 (m2 (m3 (m4 (m5 c))))
f >>>>>=> g = \x -> f x >>>>>= g
(>>>>>~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m1 (m2 (m3 (m4 (m5 a)))) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m >>>>>~ k = m >>>>>= \_ -> k

(---->=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m5 a -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m ---->= k = (.****) m >>>>>= k 
(--->-=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m4 a -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m --->-= k = (***-*) m >>>>>= k 
(-->--=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m3 a -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m -->--= k = (**-**) m >>>>>= k 
(->---=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m2 a -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m ->---= k = (*-***) m >>>>>= k 
(>----=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m1 a -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m >----= k = (-****) m >>>>>= k 

(--->>=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m4 (m5 a) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m --->>= k = (.***) m >>>>>= k 
(-->->=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m3 (m5 a) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m -->->= k = (**-*) m >>>>>= k 
(->-->=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m2 (m5 a) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m ->-->= k = (*-**) m >>>>>= k 
(>--->=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m1 (m5 a) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m >--->= k = (-***) m >>>>>= k 
(>-->-=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m1 (m4 a) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m >-->-= k = (-**-*) m >>>>>= k 
(->->-=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m2 (m4 a) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m ->->-= k = (*-*-*) m >>>>>= k 
(-->>-=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m3 (m4 a) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m -->>-= k = (**--*) m >>>>>= k 
(->>--=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m2 (m3 a) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m ->>--= k = (*--**) m >>>>>= k 
(>->--=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m1 (m3 a) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m >->--= k = (-*-**) m >>>>>= k 
(>>---=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m1 (m2 a) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m >>---= k = (--***) m >>>>>= k 

(-->>>=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m3 (m4 (m5 a)) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m -->>>= k = (.**) m >>>>>= k 
(->->>=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m2 (m4 (m5 a)) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m ->->>= k = (*-*) m >>>>>= k 
(>-->>=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m1 (m4 (m5 a)) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m >-->>= k = (-**) m >>>>>= k 
(>->->=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m1 (m3 (m5 a)) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m >->->= k = (-*-*) m >>>>>= k 
(->>->=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m2 (m3 (m5 a)) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m ->>->= k = (*--*) m >>>>>= k 
(>>-->=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m1 (m2 (m5 a)) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m >>-->= k = (--**) m >>>>>= k 
(>>->-=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m1 (m2 (m4 a)) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m >>->-= k = (--*-*) m >>>>>= k 
(>->>-=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m1 (m3 (m4 a)) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m >->>-= k = (-*--*) m >>>>>= k 
(->>>-=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m2 (m3 (m4 a)) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m ->>>-= k = (*---*) m >>>>>= k 
(>>>--=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m1 (m2 (m3 a)) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m >>>--= k = (---**) m >>>>>= k 

(->>>>=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m2 (m3 (m4 (m5 a))) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m ->>>>= k = (.*) m >>>>>= k 
(>->>>=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m1 (m3 (m4 (m5 a))) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m >->>>= k = (-*) m >>>>>= k 
(>>->>=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m1 (m2 (m4 (m5 a))) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m >>->>= k = (--*) m >>>>>= k 
(>>>->=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m1 (m2 (m3 (m5 a))) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m >>>->= k = (---*) m >>>>>= k 
(>>>>-=) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m1 (m2 (m3 (m4 a))) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m >>>>-= k = (----*) m >>>>>= k 


(---->~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m5 a -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m ---->~ k = (.****) m >>>>>~ k 
(--->-~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m4 a -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m --->-~ k = (***-*) m >>>>>~ k 
(-->--~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m3 a -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m -->--~ k = (**-**) m >>>>>~ k 
(->---~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m2 a -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m ->---~ k = (*-***) m >>>>>~ k 
(>----~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m1 a -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m >----~ k = (-****) m >>>>>~ k 

(--->>~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m4 (m5 a) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m --->>~ k = (.***) m >>>>>~ k 
(-->->~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m3 (m5 a) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m -->->~ k = (**-*) m >>>>>~ k 
(->-->~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m2 (m5 a) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m ->-->~ k = (*-**) m >>>>>~ k 
(>--->~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m1 (m5 a) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m >--->~ k = (-***) m >>>>>~ k 
(>-->-~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m1 (m4 a) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m >-->-~ k = (-**-*) m >>>>>~ k 
(->->-~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m2 (m4 a) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m ->->-~ k = (*-*-*) m >>>>>~ k 
(-->>-~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m3 (m4 a) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m -->>-~ k = (**--*) m >>>>>~ k 
(->>--~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m2 (m3 a) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m ->>--~ k = (*--**) m >>>>>~ k 
(>->--~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m1 (m3 a) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m >->--~ k = (-*-**) m >>>>>~ k 
(>>---~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m1 (m2 a) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m >>---~ k = (--***) m >>>>>~ k 

(-->>>~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m3 (m4 (m5 a)) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m -->>>~ k = (.**) m >>>>>~ k 
(->->>~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m2 (m4 (m5 a)) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m ->->>~ k = (*-*) m >>>>>~ k 
(>-->>~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m1 (m4 (m5 a)) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m >-->>~ k = (-**) m >>>>>~ k 
(>->->~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m1 (m3 (m5 a)) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m >->->~ k = (-*-*) m >>>>>~ k 
(->>->~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m2 (m3 (m5 a)) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m ->>->~ k = (*--*) m >>>>>~ k 
(>>-->~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m1 (m2 (m5 a)) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m >>-->~ k = (--**) m >>>>>~ k 
(>>->-~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m1 (m2 (m4 a)) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m >>->-~ k = (--*-*) m >>>>>~ k 
(>->>-~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m1 (m3 (m4 a)) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m >->>-~ k = (-*--*) m >>>>>~ k 
(->>>-~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m2 (m3 (m4 a)) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m ->>>-~ k = (*---*) m >>>>>~ k 
(>>>--~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m1 (m2 (m3 a)) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m >>>--~ k = (---**) m >>>>>~ k 

(->>>>~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m2 (m3 (m4 (m5 a))) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m ->>>>~ k = (.*) m >>>>>~ k 
(>->>>~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m1 (m3 (m4 (m5 a))) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m >->>>~ k = (-*) m >>>>>~ k 
(>>->>~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m1 (m2 (m4 (m5 a))) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m >>->>~ k = (--*) m >>>>>~ k 
(>>>->~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m1 (m2 (m3 (m5 a))) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m >>>->~ k = (---*) m >>>>>~ k 
(>>>>-~) :: (Monad m1, Monad m2, Traversable m2, Monad m3, Traversable m3, Monad m4, Traversable m4, Monad m5, Traversable m5) => m1 (m2 (m3 (m4 a))) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m >>>>-~ k = (----*) m >>>>>~ k 

