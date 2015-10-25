{-|
Module      : DeepControl.Monad
Description : Enable deep level Monad programming.
Copyright   : (c) 2015 KONISHI Yohsuke 
License     : BSD-style (see the LICENSE file in the distribution)
Maintainer  : ocean0yohsuke@gmail.com
Stability   : experimental
Portability : ---

This module enables you to program in Monad for more __deeper__ level than the usual 'Control.Monad' module expresses.
You would soon realize exactly what __/more deeper level/__ means by reading the example codes in order, which are attached on the Monadx(Monad2, Monad3, etc) classes below.

Note: 

    * This module never makes mlt(monad-transformer-library) unnessasary. 
      The range in which this module is helpful is regrettably confined to the range of deep monads (namely Monad2, Monad3, etc).
    
    * In my opinion this bad confinement is hard-wired with the ability of the compiler, that is to say GHC doesn't parse @(r->)@ or @((->) r)@ as a data constructor; 
      thus some fundamental expressions such as @(r->)|$>@ or @fmap (r->)@ are useless.
      Theoretically it might be impossible though.

-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module DeepControl.Monad (
    module Control.Monad,

    -- * Level-0
    -- ** bind 
    (-<), (>-), 
    -- ** composite 
    (>->), (<-<),

    -- * Level-2
    -- ** bind
    Monad2(..),
    -- ** sequence
    (>>~), 
    -- ** bind-cover
    (>-==), (->==), 
    -- ** sequence-cover
    (>-~), (->~),
    -- ** composite 
    (>==>), 

    -- * Level-3
    -- ** bind
    Monad3(..),  
    -- ** sequence
    (>>>~),
    -- ** bind-cover 
    (>>-==), (->>==), (>->==) ,(>--==),(->-==), (-->==), 
    -- ** sequence-cover 
    (>--~), (->-~), (-->~), (>>-~), (->>~), (>->~), 
    -- ** composite 
    (>===>),

    -- * Level-4
    -- ** bind
    Monad4(..), 
    -- ** sequence
    (>>>>~), 
    -- ** bind-cover 
    (--->==), (-->-==), (->--==), (>---==),
    (-->>==), (->->==), (>-->==), (>->-==), (->>-==), (>>--==),
    (->>>==), (>->>==), (>>->==), (>>>-==),
    -- ** sequence-cover 
    (--->~), (-->-~), (->--~), (>---~),
    (-->>~), (->->~), (>-->~), (>->-~), (->>-~), (>>--~),
    (->>>~), (>->>~), (>>->~), (>>>-~),
    -- ** composite 
    (>====>), 

    -- * Level-5
    -- ** bind
    Monad5(..), 
    -- ** sequence
    (>>>>>~), 
    -- ** bind-cover 
    (---->==), (--->-==), (-->--==), (->---==), (>----==),
    (--->>==), (-->->==), (->-->==), (>--->==), (>-->-==), (->->-==), (-->>-==), (->>--==), (>->--==), (>>---==),
    (-->>>==), (->->>==), (>-->>==), (>->->==), (->>->==), (>>-->==), (>>->-==), (>->>-==), (->>>-==), (>>>--==),
    (->>>>==), (>->>>==), (>>->>==), (>>>->==), (>>>>-==),
    -- ** sequence-cover 
    (---->~), (--->-~), (-->--~), (->---~), (>----~),
    (--->>~), (-->->~), (->-->~), (>--->~), (>-->-~), (->->-~), (-->>-~), (->>--~), (>->--~), (>>---~),
    (-->>>~), (->->>~), (>-->>~), (>->->~), (->>->~), (>>-->~), (>>->-~), (>->>-~), (->>>-~), (>>>--~),
    (->>>>~), (>->>>~), (>>->>~), (>>>->~), (>>>>-~),
    -- ** composite 
    (>=====>), 
    
    ) where 

import DeepControl.Applicative

import Control.Monad
import Control.Monad.Writer (Writer, WriterT(..), runWriter)
import Control.Monad.Except (Except, ExceptT(..), runExcept)
import Data.Functor.Identity
import Data.Monoid (Monoid, (<>))

-------------------------------------------------------------------------------
-- Level-0 functions

infixl 1  -<, >-
-- | Anologous for @'$'@; (-<) is left associative.
--
-- >>> Just -< 3
-- Just 3
(-<) :: (a -> b) -> a -> b
(-<) = ($)
-- | The auguments-flipped function for @'-<'@.
--
-- >>> 3 >- Just
-- Just 3
--
-- >>> :{
-- let plus :: Int -> Int -> Int
--     plus x y = 
--         x >- \a ->  -- (>-) is the level-0 bind function, analogous for (>>=)
--         y >- \b ->
--         a + b
-- in plus 3 4
-- :}
-- 7
--
(>-) :: a -> (a -> b) -> b
(>-) = flip (-<)

infixr 1  <-<, >->
-- | Anologous for @'.'@. 
--
-- >>> ((3+) <-< (2*) <-< (1+)) -< 1
-- 7
(<-<) :: (b -> c) -> (a -> b) -> a -> c
(<-<) = (.)
-- | The auguments-flipped function for @'<-<'@. 
--
-- >>> 1 >- ((+1) >-> (*2) >-> (+3))
-- 7
(>->) :: (a -> b) -> (b -> c) -> a -> c
(>->) = flip (<-<)

-------------------------------------------------------------------------------
-- Level-1 functions

-------------------------------------------------------------------------------
-- Level-2 functions

infixr 1  >==>
infixr 1  >>==, >>~
infixr 1  ->==, >-==
infixr 1  ->~, >-~

-- | The 'Monad2' class defines the Monad function for level-2 types @m1 (m2 a)@; such as [[a]], Maybe [a], Either () (Maybe a), a -> [b], IO [a], etc.
-- 
-- >>> :{
--  -- List-List monad
--  [["a","b"]] >>== \x ->   -- (>>==) is the level-2 bind function, analogous for (>>=)
--  [[0],[1,2]] >>== \y -> 
--  (**:) (x ++ show y)
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
--          (**:) (fromIntegral s / fromIntegral l)
--  in [averageM [10, 25, 70], averageM []]
-- :}
-- [Just 35.0,Nothing]
-- 
class (Monad m2) => Monad2 m2 where
  -- | The level-2 bind function.
  (>>==) :: (Monad m1) => m1 (m2 a) -> (a -> m1 (m2 b)) -> m1 (m2 b)

(>==>) :: (Monad m1, Monad2 m2) => (a -> m1 (m2 b)) -> (b -> m1 (m2 c)) -> a -> m1 (m2 c)
f >==> g = \x -> f x >>== g
(>>~) :: (Monad m1, Monad2 m2) => m1 (m2 a) -> m1 (m2 b) -> m1 (m2 b)
m >>~ k = m >>== \_ -> k
(>-==) :: (Monad m1, Monad2 m2) => m1 a -> (a -> m1 (m2 b)) -> m1 (m2 b)
m >-== k = (-*) m >>== k
(->==) :: (Monad m1, Monad2 m2) => m2 a -> (a -> m1 (m2 b)) -> m1 (m2 b)
m ->== k = (*:) m >>== k
(>-~) :: (Monad m1, Monad2 m2) => m1 a -> m1 (m2 b) -> m1 (m2 b)
m >-~ k = (-*) m >>~ k
(->~) :: (Monad m1, Monad2 m2) => m2 a -> m1 (m2 b) -> m1 (m2 b)
m ->~ k = (*:) m >>~ k

instance Monad2 Maybe where
    mv >>== f = 
        mv >>= \mv ->
        case mv of 
            Nothing -> (*:) Nothing
            Just a  -> f a

instance Monad2 [] where
    mv >>== f = 
        mv >>= \xs -> 
        foldr (\x acc -> f x <$|(++)|*> acc) ((*:) []) xs

instance Monad2 (Either e) where
    mv >>== f = 
        mv >>= \mv -> 
        case mv of
            Left l  -> (*:) (Left l)
            Right r -> f r

instance Monad2 (Except e) where
    m >>== f = (ExceptT . Identity |$>) $ (runExcept |$> m) >>== runExcept |$>> f

instance (Monoid w) => Monad2 (Writer w) where
    mv >>== f = 
        mv >>= \x -> runWriterT x >- \(Identity (a, w)) ->
        f a <$| (\x -> runWriterT x >- \(Identity (b, w')) ->
                       WriterT $ Identity (b, w <> w'))


-------------------------------------------------------------------------------
-- Level-3 functions

infixr 1  >===>
infixr 1  >>>==, >>>~
infixr 1  >--==, ->-==, -->==, >>-==, >->==, ->>==
infixr 1  >--~, ->-~, -->~, >>-~, >->~, ->>~

-- | The 'Monad3' class defines the Monad function for level-3 types @m1 (m2 (m3 a)@.
-- 
-- >>> :{
--  -- IO-List-List monad
--  (*:) [["a","b"]] >>>== \x ->   -- (>>>==) is the level-3 bind-cover function, analogous for (>>=)
--  (*:) [[0],[1,2]] >>>== \y ->
--  (-**) (print (x,y)) >>>~       -- (>>>~) is the level-3 sequence function, analogous for (>>)
--  (***:) (x ++ show y)
-- :}
-- ("a",0)
-- ("a",1)
-- ("a",2)
-- ("b",0)
-- ("b",1)
-- ("b",2)
-- [["a0","b0"],["a0","b1","b2"],["a1","a2","b0"],["a1","a2","b1","b2"]]
--
-- This messy code above can be neatly rewritten to the code below.
--
-- >>> :{
--  -- IO-List-List monad
--  [["a","b"]] ->>== \x ->   -- (->>==) is a level-3 bind-cover function, analogous for (>>=)
--  [[0],[1,2]] ->>== \y ->
--  print (x,y) >--~          -- (>--~) is a level-3 bind-cover function, analogous for (>>)
--  (***:) (x ++ show y)
-- :}
-- ("a",0)
-- ("a",1)
-- ("a",2)
-- ("b",0)
-- ("b",1)
-- ("b",2)
-- [["a0","b0"],["a0","b1","b2"],["a1","a2","b0"],["a1","a2","b1","b2"]]
--
class (Monad2 m3) => Monad3 m3 where
  (>>>==) :: (Monad m1, Monad2 m2) => m1 (m2 (m3 a)) -> (a -> m1 (m2 (m3 b))) -> m1 (m2 (m3 b))

(>===>) :: (Monad m1, Monad2 m2, Monad3 m3) => (a -> m1 (m2 (m3 b))) -> (b -> m1 (m2 (m3 c))) -> a -> m1 (m2 (m3 c))
f >===> g = \x -> f x >>>== g
(>>>~) :: (Monad m1, Monad2 m2, Monad3 m3) => m1 (m2 (m3 a)) -> m1 (m2 (m3 b)) -> m1 (m2 (m3 b))
m >>>~ k = m >>>== \_ -> k

(>--==) :: (Monad m1, Monad2 m2, Monad3 m3) => m1 a -> (a -> m1 (m2 (m3 b))) -> m1 (m2 (m3 b))
m >--== k = (-**) m >>>== k
(->-==) :: (Monad m1, Monad2 m2, Monad3 m3) => m2 a -> (a -> m1 (m2 (m3 b))) -> m1 (m2 (m3 b))
m ->-== k = (*-*) m >>>== k
(-->==) :: (Monad m1, Monad2 m2, Monad3 m3) => m3 a -> (a -> m1 (m2 (m3 b))) -> m1 (m2 (m3 b))
m -->== k = (**:) m >>>== k
(>>-==) :: (Monad m1, Monad2 m2, Monad3 m3) => m1 (m2 a) -> (a -> m1 (m2 (m3 b))) -> m1 (m2 (m3 b))
m >>-== k = (--*) m >>>== k
(->>==) :: (Monad m1, Monad2 m2, Monad3 m3) => m2 (m3 a) -> (a -> m1 (m2 (m3 b))) -> m1 (m2 (m3 b))
m ->>== k = (*:) m >>>== k
(>->==) :: (Monad m1, Monad2 m2, Monad3 m3) => m1 (m3 a) -> (a -> m1 (m2 (m3 b))) -> m1 (m2 (m3 b))
m >->== k = (-*) m >>>== k
(-->~) :: (Monad m1, Monad2 m2, Monad3 m3) => m3 a -> m1 (m2 (m3 b)) -> m1 (m2 (m3 b))
m -->~ k = (**:) m >>>~ k
(->-~) :: (Monad m1, Monad2 m2, Monad3 m3) => m2 a -> m1 (m2 (m3 b)) -> m1 (m2 (m3 b))
m ->-~ k = (*-*) m >>>~ k
(>--~) :: (Monad m1, Monad2 m2, Monad3 m3) => m1 a -> m1 (m2 (m3 b)) -> m1 (m2 (m3 b))
m >--~ k = (-**) m >>>~ k
(->>~) :: (Monad m1, Monad2 m2, Monad3 m3) => m2 (m3 a) -> m1 (m2 (m3 b)) -> m1 (m2 (m3 b))
m ->>~ k = (*:) m >>>~ k
(>->~) :: (Monad m1, Monad2 m2, Monad3 m3) => m1 (m3 a) -> m1 (m2 (m3 b)) -> m1 (m2 (m3 b))
m >->~ k = (-*) m >>>~ k
(>>-~) :: (Monad m1, Monad2 m2, Monad3 m3) => m1 (m2 a) -> m1 (m2 (m3 b)) -> m1 (m2 (m3 b))
m >>-~ k = (--*) m >>>~ k

instance Monad3 Maybe where
    mv >>>== f = 
        mv >>== \mv ->
        case mv of 
            Nothing -> (**:) Nothing
            Just a  -> f a

instance Monad3 [] where
    mv >>>== f = 
        mv >>== \xs -> 
        foldr (\x acc -> f x <<$|(++)|*>> acc) ((**:) []) xs 

instance Monad3 (Either e) where
    mv >>>== f = 
        mv >>== \mv -> 
        case mv of
            Left l  -> (**:) (Left l)
            Right r -> f r

instance Monad3 (Except e) where
    m >>>== f = (ExceptT . Identity |$>>) $ (runExcept |$>> m) >>>== runExcept |$>>> f

instance (Monoid w) => Monad3 (Writer w) where
    mv >>>== f = 
        mv >>== \x -> runWriterT x >- \(Identity (a, w)) ->
        f a <<$| (\x -> runWriterT x >- \(Identity (b, w')) ->
                        WriterT $ Identity (b, w <> w'))


-------------------------------------------------------------------------------
-- Level-4 functions

infixr 1  >====>
infixr 1  >>>>==, >>>>~
infixr 1  --->==, -->-==, ->--==, >---==
infixr 1  -->>==, ->->==, >-->==, >->-==, ->>-==, >>--==
infixr 1  ->>>==, >->>==, >>->==, >>>-==
infixr 1  --->~, -->-~, ->--~, >---~
infixr 1  -->>~, ->->~, >-->~, >->-~, ->>-~, >>--~
infixr 1  ->>>~, >->>~, >>->~, >>>-~

class (Monad3 m4) => Monad4 m4 where
  (>>>>==) :: (Monad m1, Monad2 m2, Monad3 m3) => m1 (m2 (m3 (m4 a))) -> (a -> m1 (m2 (m3 (m4 b)))) -> m1 (m2 (m3 (m4 b)))

(>====>) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => (a -> m1 (m2 (m3 (m4 b)))) -> (b -> m1 (m2 (m3 (m4 c)))) -> a -> m1 (m2 (m3 (m4 c)))
f >====> g = \x -> f x >>>>== g
(>>>>~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m1 (m2 (m3 (m4 a))) -> m1 (m2 (m3 (m4 b))) -> m1 (m2 (m3 (m4 b)))
m >>>>~ k = m >>>>== \_ -> k

(--->==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m4 a -> (a -> m1 (m2 (m3 (m4 b)))) -> m1 (m2 (m3 (m4 b)))
m --->== k = (***:) m >>>>== k 
(-->-==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m3 a -> (a -> m1 (m2 (m3 (m4 b)))) -> m1 (m2 (m3 (m4 b)))
m -->-== k = (**-*) m >>>>== k 
(->--==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m2 a -> (a -> m1 (m2 (m3 (m4 b)))) -> m1 (m2 (m3 (m4 b)))
m ->--== k = (*-**) m >>>>== k 
(>---==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m1 a -> (a -> m1 (m2 (m3 (m4 b)))) -> m1 (m2 (m3 (m4 b)))
m >---== k = (-***) m >>>>== k

(-->>==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m3 (m4 a) -> (a -> m1 (m2 (m3 (m4 b)))) -> m1 (m2 (m3 (m4 b)))
m -->>== k = (**:) m >>>>== k 
(->->==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m2 (m4 a) -> (a -> m1 (m2 (m3 (m4 b)))) -> m1 (m2 (m3 (m4 b)))
m ->->== k = (*-*) m >>>>== k 
(>-->==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m1 (m4 a) -> (a -> m1 (m2 (m3 (m4 b)))) -> m1 (m2 (m3 (m4 b)))
m >-->== k = (-**) m >>>>== k 
(>->-==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m1 (m3 a) -> (a -> m1 (m2 (m3 (m4 b)))) -> m1 (m2 (m3 (m4 b)))
m >->-== k = (-*-*) m >>>>== k 
(->>-==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m2 (m3 a) -> (a -> m1 (m2 (m3 (m4 b)))) -> m1 (m2 (m3 (m4 b)))
m ->>-== k = (*--*) m >>>>== k 
(>>--==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m1 (m2 a) -> (a -> m1 (m2 (m3 (m4 b)))) -> m1 (m2 (m3 (m4 b)))
m >>--== k = (--**) m >>>>== k 

(->>>==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m2 (m3 (m4 a)) -> (a -> m1 (m2 (m3 (m4 b)))) -> m1 (m2 (m3 (m4 b)))
m ->>>== k = (*:) m >>>>== k 
(>->>==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m1 (m3 (m4 a)) -> (a -> m1 (m2 (m3 (m4 b)))) -> m1 (m2 (m3 (m4 b)))
m >->>== k = (-*) m >>>>== k 
(>>->==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m1 (m2 (m4 a)) -> (a -> m1 (m2 (m3 (m4 b)))) -> m1 (m2 (m3 (m4 b)))
m >>->== k = (--*) m >>>>== k 
(>>>-==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m1 (m2 (m3 a)) -> (a -> m1 (m2 (m3 (m4 b)))) -> m1 (m2 (m3 (m4 b)))
m >>>-== k = (---*) m >>>>== k 

(--->~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m4 a -> m1 (m2 (m3 (m4 b))) -> m1 (m2 (m3 (m4 b)))
m --->~ k = (***:) m >>>>~ k
(-->-~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m3 a -> m1 (m2 (m3 (m4 b))) -> m1 (m2 (m3 (m4 b)))
m -->-~ k = (**-*) m >>>>~ k
(->--~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m2 a -> m1 (m2 (m3 (m4 b))) -> m1 (m2 (m3 (m4 b)))
m ->--~ k = (*-**) m >>>>~ k
(>---~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m1 a -> m1 (m2 (m3 (m4 b))) -> m1 (m2 (m3 (m4 b)))
m >---~ k = (-***) m >>>>~ k

(-->>~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m3 (m4 a) -> m1 (m2 (m3 (m4 b))) -> m1 (m2 (m3 (m4 b)))
m -->>~ k = (**:) m >>>>~ k
(->->~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m2 (m4 a) -> m1 (m2 (m3 (m4 b))) -> m1 (m2 (m3 (m4 b)))
m ->->~ k = (*-*) m >>>>~ k
(>-->~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m1 (m4 a) -> m1 (m2 (m3 (m4 b))) -> m1 (m2 (m3 (m4 b)))
m >-->~ k = (-**) m >>>>~ k
(>->-~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m1 (m3 a) -> m1 (m2 (m3 (m4 b))) -> m1 (m2 (m3 (m4 b)))
m >->-~ k = (-*-*) m >>>>~ k
(->>-~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m2 (m3 a) -> m1 (m2 (m3 (m4 b))) -> m1 (m2 (m3 (m4 b)))
m ->>-~ k = (*--*) m >>>>~ k
(>>--~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m1 (m2 a) -> m1 (m2 (m3 (m4 b))) -> m1 (m2 (m3 (m4 b)))
m >>--~ k = (--**) m >>>>~ k

(->>>~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m2 (m3 (m4 a)) -> m1 (m2 (m3 (m4 b))) -> m1 (m2 (m3 (m4 b)))
m ->>>~ k = (*:) m >>>>~ k
(>->>~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m1 (m3 (m4 a)) -> m1 (m2 (m3 (m4 b))) -> m1 (m2 (m3 (m4 b)))
m >->>~ k = (-*) m >>>>~ k
(>>->~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m1 (m2 (m4 a)) -> m1 (m2 (m3 (m4 b))) -> m1 (m2 (m3 (m4 b)))
m >>->~ k = (--*) m >>>>~ k
(>>>-~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m1 (m2 (m3 a)) -> m1 (m2 (m3 (m4 b))) -> m1 (m2 (m3 (m4 b)))
m >>>-~ k = (---*) m >>>>~ k

instance Monad4 Maybe where
    mv >>>>== f = 
        mv >>>== \mv ->
        case mv of 
            Nothing -> (***:) Nothing
            Just a  -> f a

instance Monad4 [] where
    mv >>>>== f = 
        mv >>>== \xs -> 
        foldr (\x acc -> f x <<<$|(++)|*>>> acc) ((***:) []) xs 

instance Monad4 (Either e) where
    mv >>>>== f = 
        mv >>>== \mv -> 
        case mv of
            Left l  -> (***:) (Left l)
            Right r -> f r

instance Monad4 (Except e) where
    m >>>>== f = (ExceptT . Identity |$>>>) $ (runExcept |$>>> m) >>>>== runExcept |$>>>> f

instance (Monoid w) => Monad4 (Writer w) where
    mv >>>>== f = 
        mv >>>== \x -> runWriterT x >- \(Identity (a, w)) ->
        f a <<<$| (\x -> runWriterT x >- \(Identity (b, w')) ->
                         WriterT $ Identity (b, w <> w'))


-------------------------------------------------------------------------------
-- Level-5 functions

infixr 1  >=====>
infixr 1  >>>>>~, >>>>>==
infixr 1  ---->==, --->-==, -->--==, ->---==, >----==
infixr 1  --->>==, -->->==, ->-->==, >--->==, >-->-==, ->->-==, -->>-==, ->>--==, >->--==, >>---==
infixr 1  -->>>==, ->->>==, >-->>==, >->->==, ->>->==, >>-->==, >>->-==, >->>-==, ->>>-==, >>>--==
infixr 1  ->>>>==, >->>>==, >>->>==, >>>->==, >>>>-==

infixr 1  ---->~, --->-~, -->--~, ->---~, >----~
infixr 1  --->>~, -->->~, ->-->~, >--->~, >-->-~, ->->-~, -->>-~, ->>--~, >->--~, >>---~
infixr 1  -->>>~, ->->>~, >-->>~, >->->~, ->>->~, >>-->~, >>->-~, >->>-~, ->>>-~, >>>--~
infixr 1  ->>>>~, >->>>~, >>->>~, >>>->~, >>>>-~

class (Monad4 m5) => Monad5 m5 where
  (>>>>>==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m1 (m2 (m3 (m4 (m5 a)))) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))

(>=====>) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => (a -> m1 (m2 (m3 (m4 (m5 b))))) -> (b -> m1 (m2 (m3 (m4 (m5 c))))) -> a -> m1 (m2 (m3 (m4 (m5 c))))
f >=====> g = \x -> f x >>>>>== g
(>>>>>~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m1 (m2 (m3 (m4 (m5 a)))) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m >>>>>~ k = m >>>>>== \_ -> k

(---->==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m5 a -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m ---->== k = (****:) m >>>>>== k 
(--->-==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m4 a -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m --->-== k = (***-*) m >>>>>== k 
(-->--==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m3 a -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m -->--== k = (**-**) m >>>>>== k 
(->---==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m2 a -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m ->---== k = (*-***) m >>>>>== k 
(>----==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m1 a -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m >----== k = (-****) m >>>>>== k 

(--->>==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m4 (m5 a) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m --->>== k = (***:) m >>>>>== k 
(-->->==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m3 (m5 a) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m -->->== k = (**-*) m >>>>>== k 
(->-->==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m2 (m5 a) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m ->-->== k = (*-**) m >>>>>== k 
(>--->==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m1 (m5 a) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m >--->== k = (-***) m >>>>>== k 
(>-->-==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m1 (m4 a) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m >-->-== k = (-**-*) m >>>>>== k 
(->->-==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m2 (m4 a) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m ->->-== k = (*-*-*) m >>>>>== k 
(-->>-==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m3 (m4 a) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m -->>-== k = (**--*) m >>>>>== k 
(->>--==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m2 (m3 a) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m ->>--== k = (*--**) m >>>>>== k 
(>->--==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m1 (m3 a) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m >->--== k = (-*-**) m >>>>>== k 
(>>---==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m1 (m2 a) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m >>---== k = (--***) m >>>>>== k 

(-->>>==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m3 (m4 (m5 a)) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m -->>>== k = (**:) m >>>>>== k 
(->->>==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m2 (m4 (m5 a)) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m ->->>== k = (*-*) m >>>>>== k 
(>-->>==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m1 (m4 (m5 a)) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m >-->>== k = (-**) m >>>>>== k 
(>->->==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m1 (m3 (m5 a)) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m >->->== k = (-*-*) m >>>>>== k 
(->>->==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m2 (m3 (m5 a)) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m ->>->== k = (*--*) m >>>>>== k 
(>>-->==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m1 (m2 (m5 a)) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m >>-->== k = (--**) m >>>>>== k 
(>>->-==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m1 (m2 (m4 a)) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m >>->-== k = (--*-*) m >>>>>== k 
(>->>-==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m1 (m3 (m4 a)) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m >->>-== k = (-*--*) m >>>>>== k 
(->>>-==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m2 (m3 (m4 a)) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m ->>>-== k = (*---*) m >>>>>== k 
(>>>--==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m1 (m2 (m3 a)) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m >>>--== k = (---**) m >>>>>== k 

(->>>>==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m2 (m3 (m4 (m5 a))) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m ->>>>== k = (*:) m >>>>>== k 
(>->>>==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m1 (m3 (m4 (m5 a))) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m >->>>== k = (-*) m >>>>>== k 
(>>->>==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m1 (m2 (m4 (m5 a))) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m >>->>== k = (--*) m >>>>>== k 
(>>>->==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m1 (m2 (m3 (m5 a))) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m >>>->== k = (---*) m >>>>>== k 
(>>>>-==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m1 (m2 (m3 (m4 a))) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))
m >>>>-== k = (----*) m >>>>>== k 


(---->~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m5 a -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m ---->~ k = (****:) m >>>>>~ k 
(--->-~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m4 a -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m --->-~ k = (***-*) m >>>>>~ k 
(-->--~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m3 a -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m -->--~ k = (**-**) m >>>>>~ k 
(->---~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m2 a -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m ->---~ k = (*-***) m >>>>>~ k 
(>----~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m1 a -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m >----~ k = (-****) m >>>>>~ k 

(--->>~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m4 (m5 a) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m --->>~ k = (***:) m >>>>>~ k 
(-->->~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m3 (m5 a) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m -->->~ k = (**-*) m >>>>>~ k 
(->-->~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m2 (m5 a) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m ->-->~ k = (*-**) m >>>>>~ k 
(>--->~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m1 (m5 a) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m >--->~ k = (-***) m >>>>>~ k 
(>-->-~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m1 (m4 a) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m >-->-~ k = (-**-*) m >>>>>~ k 
(->->-~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m2 (m4 a) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m ->->-~ k = (*-*-*) m >>>>>~ k 
(-->>-~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m3 (m4 a) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m -->>-~ k = (**--*) m >>>>>~ k 
(->>--~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m2 (m3 a) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m ->>--~ k = (*--**) m >>>>>~ k 
(>->--~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m1 (m3 a) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m >->--~ k = (-*-**) m >>>>>~ k 
(>>---~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m1 (m2 a) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m >>---~ k = (--***) m >>>>>~ k 

(-->>>~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m3 (m4 (m5 a)) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m -->>>~ k = (**:) m >>>>>~ k 
(->->>~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m2 (m4 (m5 a)) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m ->->>~ k = (*-*) m >>>>>~ k 
(>-->>~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m1 (m4 (m5 a)) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m >-->>~ k = (-**) m >>>>>~ k 
(>->->~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m1 (m3 (m5 a)) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m >->->~ k = (-*-*) m >>>>>~ k 
(->>->~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m2 (m3 (m5 a)) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m ->>->~ k = (*--*) m >>>>>~ k 
(>>-->~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m1 (m2 (m5 a)) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m >>-->~ k = (--**) m >>>>>~ k 
(>>->-~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m1 (m2 (m4 a)) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m >>->-~ k = (--*-*) m >>>>>~ k 
(>->>-~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m1 (m3 (m4 a)) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m >->>-~ k = (-*--*) m >>>>>~ k 
(->>>-~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m2 (m3 (m4 a)) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m ->>>-~ k = (*---*) m >>>>>~ k 
(>>>--~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m1 (m2 (m3 a)) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m >>>--~ k = (---**) m >>>>>~ k 

(->>>>~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m2 (m3 (m4 (m5 a))) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m ->>>>~ k = (*:) m >>>>>~ k 
(>->>>~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m1 (m3 (m4 (m5 a))) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m >->>>~ k = (-*) m >>>>>~ k 
(>>->>~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m1 (m2 (m4 (m5 a))) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m >>->>~ k = (--*) m >>>>>~ k 
(>>>->~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m1 (m2 (m3 (m5 a))) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m >>>->~ k = (---*) m >>>>>~ k 
(>>>>-~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m1 (m2 (m3 (m4 a))) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m >>>>-~ k = (----*) m >>>>>~ k 

instance Monad5 Maybe where
    mv >>>>>== f = 
        mv >>>>== \mv ->
        case mv of 
            Nothing -> (****:) Nothing
            Just a  -> f a

instance Monad5 [] where
    mv >>>>>== f = 
        mv >>>>== \xs -> 
        foldr (\x acc -> f x <<<<$|(++)|*>>>> acc) ((****:) []) xs 

instance Monad5 (Either e) where
    mv >>>>>== f = 
        mv >>>>== \mv -> 
        case mv of
            Left l  -> (****:) (Left l)
            Right r -> f r

instance Monad5 (Except e) where
    m >>>>>== f = (ExceptT . Identity |$>>>>) $ (runExcept |$>>>> m) >>>>>== runExcept |$>>>>> f

instance (Monoid w) => Monad5 (Writer w) where
    mv >>>>>== f = 
        mv >>>>== \x -> runWriterT x >- \(Identity (a, w)) ->
        f a <<<<$| (\x -> runWriterT x >- \(Identity (b, w')) ->
                          WriterT $ Identity (b, w <> w'))


