{-|
Module      : DeepControl.Monad
Description : Enable deep level Monad programming.
Copyright   : KONISHI Yohuske 2015
License     : BSD-style (see the LICENSE file in the distribution)
Maintainer  : ocean0yohsuke@gmail.com
Stability   : experimental
Portability : ---

This module enables you to program in Monad for more __deeper__ level than the usual 'Control.Monad' module expresses.
You would soon realize exactly what __/more deeper level/__ means by reading the example codes in order, which are attached on the Monadx(Monad2, Monad3, etc) classes below.

Note: 

    * Though this module substitutes for monad-transform consisted of elemental(without-lambda-expression) kind of monad, however, this module is unable to substitute for monad-transform consisted of complicated(tangled-with-lambda-expression) kind of monad at all.
      So this module does not thoroughly make mlt(monad-transformer-library) unnessasary. 
      The range in which this module is helpful is confined to the range that single Monad or Monad with Monadx series (namely Monad2, Monad3, etc) defines.
    
    * In my opinion the above problem is hard-wired with the ability of the compiler, that is to say GHC doesn't parse @(r->)@ or @((->) r)@ as a data constructor; 
      thus some fundamental expressions such as @(r->)|$>@ or @fmap (r->)@ are useless.
      Theoretically it might be impossible though.

-}
module DeepControl.Monad (
    module Control.Monad,

    -- * Level-0
    -- ** bind function
    (-<), (>-), 
    -- ** composite function
    (>->), (<-<),

    -- * Level-1
    -- ** bind-sequence function
    (<<),

    -- * Level-2
    Monad2(..),
    -- ** bind-sequence function
    (>>~), (>-==), (->==), (>-~), (->~),
    -- ** composite function
    (>==>), 

    -- * Level-3
    Monad3(..),
    -- ** bind-sequence function
    (>>-==), (->>==), (>->==) ,(>--==),(->-==), (-->==), 
    (>>>~), (->-~), (-->~), (>>-~), (->>~), (>->~), (>--~),
    -- ** composite function
    (>===>),

    -- * Level-4
    Monad4(..),
    -- ** bind-sequence function
    (>>>>~), 
    -- ** composite function
    (>====>), 

    -- * Level-5
    Monad5(..),
    -- ** bind-sequence function
    (>>>>>~), 
    -- ** composite function
    (>=====>), 
    
    ) where 

import DeepControl.Applicative
import Control.Monad

-- -----------------------------------------------------------------------------
-- Level-0 functions

infixl 1  -<, >-
-- | Alias for @'$'@.
--
-- >>> Just -< 3
-- Just 3
(-<) :: (a -> b) -> a -> b
(-<) = ($)
-- | The auguments-flipped function for @'-<'@.
--
-- >>> 3 >- Just
-- Just 3
(>-) :: a -> (a -> b) -> b
(>-) = flip (-<)

infixr 1  <-<, >->
-- | Alias for @'.'@. 
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

-- -----------------------------------------------------------------------------
-- Level-1 functions

infixr 1  <<
-- | The auguments-flipped function for @'>>'@. 
(<<) :: Monad m => m b -> m a -> m b 
(<<) = flip (>>)

-- -----------------------------------------------------------------------------
-- Level-2 functions

infixr 1  >==>
infixr 1  >>~, >>==
infixr 1  ->==, >-==
infixr 1  ->~, >-~

-- | The 'Monad2' class defines the Monad functions for level-2 types @m1 (m2 a)@; such as [[a]], Maybe [a], Either () (Maybe a), a -> [b], IO [a], etc.
-- 
-- >>> :{
--  [["a","b"]] >>== \x -> 
--  [[0],[1,2]] >>== \y -> 
--  (**:) $ x ++ show y
-- :}
-- [["a0","b0"],["a0","b1","b2"],["a1","a2","b0"],["a1","a2","b1","b2"]]
--
-- >>> :{
--  let 
--    isJust (Just _) = True
--    isJust _        = False
--    pythagorean_triple :: [Maybe (Int, Int, Int)]  -- List-Maybe Monad
--    pythagorean_triple = filter isJust $
--        [1..10] >-== \x ->
--        [1..10] >-== \y ->
--        [1..10] >-== \z ->
--        guard (x < y && x*x + y*y == z*z) ->~
--        (**:) (x,y,z)
--  in pythagorean_triple
-- :}
-- [Just (3,4,5),Just (6,8,10)]
-- 
-- 
class (Monad m2) => Monad2 m2 where
  -- | Bind function of level-2.
  (>>==) :: (Monad m1) => m1 (m2 a) -> (a -> m1 (m2 b)) -> m1 (m2 b)

-- | Composite function of level-2.
(>==>) :: (Monad m1, Monad2 m2) => (a -> m1 (m2 b)) -> (b -> m1 (m2 c)) -> a -> m1 (m2 c)
f >==> g = \x -> f x >>== g
-- | Sequence function of level-2.
(>>~) :: (Monad m1, Monad2 m2) => m1 (m2 a) -> m1 (m2 b) -> m1 (m2 b)
m >>~ k = m >>== \_ -> k
-- | Bind-cover function made of bind @'>>=='@ and cover @'-*'@, defined as @m >-== k = (-*) m >>== k@.
(>-==) :: (Monad m1, Monad2 m2) => m1 a -> (a -> m1 (m2 b)) -> m1 (m2 b)
m >-== k = (-*) m >>== k
-- | Bind-cover function made of bind @'>>=='@ and cover @'*-'@, defined as @m >-== k = (*-) m >>== k@.
(->==) :: (Monad m1, Monad2 m2) => m2 a -> (a -> m1 (m2 b)) -> m1 (m2 b)
m ->== k = (*-) m >>== k
-- | Sequence-cover function made of sequence @'>>~'@ and cover @'-*'@, defined as @m >-~ k = (-*) m >>~ k@.
(>-~) :: (Monad m1, Monad2 m2) => m1 a -> m1 (m2 b) -> m1 (m2 b)
m >-~ k = (-*) m >>~ k
-- | Sequence-cover function made of sequence @'>>~'@ and cover @'*-'@, defined as @m >-~ k = (*-) m >>~ k@.
(->~) :: (Monad m1, Monad2 m2) => m2 a -> m1 (m2 b) -> m1 (m2 b)
m ->~ k = (*-) m >>~ k

instance Monad2 Maybe where
    mmv >>== f = 
        mmv >>= \mv ->
        case mv of 
            Nothing -> (*:) Nothing
            Just a  -> f a

instance Monad2 [] where
    mmv >>== f = 
        mmv >>= \xs -> 
        foldr (\x acc -> f x <$|(++)|*> acc) ((*:) []) xs

instance Monad2 (Either e) where
    mmv >>== f = 
        mmv >>= \mv -> 
        case mv of
            Left l  -> (*:) (Left l)
            Right r -> f r

-- -----------------------------------------------------------------------------
-- Level-3 functions

infixr 1  >===>
infixr 1  >>>~, >>>==
infixr 1  >--==, ->-==, -->==, >>-==, >->==, ->>==
infixr 1  >--~, ->-~, -->~, >>-~, >->~, ->>~

-- | The 'Monad3' class defines the Monad functions for level-3 types @m1 (m2 (m3 a)@.
-- 
-- >>> :{
--  let 
--    isJust (Just _) = True
--    isJust _        = False
--    pythagorean_triple :: IO [Maybe (Int, Int, Int)]  -- IO-List-Maybe Monad
--    pythagorean_triple = filter isJust |$> (
--        [1..10] ->-== \x ->
--        [1..10] ->-== \y ->
--        [1..10] ->-== \z ->
--        guard (x < y && x*x + y*y == z*z) -->~
--        print (x,y,z) >--~
--        (***:) (x,y,z)
--      )
--  in pythagorean_triple
-- :}
-- (3,4,5)
-- (6,8,10)
-- [Just (3,4,5),Just (6,8,10)]
--
class (Monad m3) => Monad3 m3 where
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
m -->== k = (**-) m >>>== k
(>>-==) :: (Monad m1, Monad2 m2, Monad3 m3) => m1 (m2 a) -> (a -> m1 (m2 (m3 b))) -> m1 (m2 (m3 b))
m >>-== k = (--*) m >>>== k
(->>==) :: (Monad m1, Monad2 m2, Monad3 m3) => m2 (m3 a) -> (a -> m1 (m2 (m3 b))) -> m1 (m2 (m3 b))
m ->>== k = (*--) m >>>== k
(>->==) :: (Monad m1, Monad2 m2, Monad3 m3) => m1 (m3 a) -> (a -> m1 (m2 (m3 b))) -> m1 (m2 (m3 b))
m >->== k = (-*-) m >>>== k
(>--~) :: (Monad m1, Monad2 m2, Monad3 m3) => m1 a -> m1 (m2 (m3 b)) -> m1 (m2 (m3 b))
m >--~ k = (-**) m >>>~ k
(->-~) :: (Monad m1, Monad2 m2, Monad3 m3) => m2 a -> m1 (m2 (m3 b)) -> m1 (m2 (m3 b))
m ->-~ k = (*-*) m >>>~ k
(-->~) :: (Monad m1, Monad2 m2, Monad3 m3) => m3 a -> m1 (m2 (m3 b)) -> m1 (m2 (m3 b))
m -->~ k = (**-) m >>>~ k
(>>-~) :: (Monad m1, Monad2 m2, Monad3 m3) => m1 (m2 a) -> m1 (m2 (m3 b)) -> m1 (m2 (m3 b))
m >>-~ k = (--*) m >>>~ k
(->>~) :: (Monad m1, Monad2 m2, Monad3 m3) => m2 (m3 a) -> m1 (m2 (m3 b)) -> m1 (m2 (m3 b))
m ->>~ k = (*--) m >>>~ k
(>->~) :: (Monad m1, Monad2 m2, Monad3 m3) => m1 (m3 a) -> m1 (m2 (m3 b)) -> m1 (m2 (m3 b))
m >->~ k = (-*-) m >>>~ k

instance Monad3 Maybe where
    mmmv >>>== f = 
        mmmv >>== \mv ->
        case mv of 
            Nothing -> (**:) Nothing
            Just a  -> f a

instance Monad3 [] where
    mmmv >>>== f = 
        mmmv >>== \xs -> 
        foldr (\x acc -> f x <<$|(++)|*>> acc) ((**:) []) xs 

instance Monad3 (Either e) where
    mmmv >>>== f = 
        mmmv >>== \mv -> 
        case mv of
            Left l  -> (**:) (Left l)
            Right r -> f r

-- -----------------------------------------------------------------------------
-- Level-4 functions

infixr 1  >====>
infixr 1  >>>>~, >>>>==
-- TODO: >>>>~

class (Monad m4) => Monad4 m4 where
  (>>>>==) :: (Monad m1, Monad2 m2, Monad3 m3) => m1 (m2 (m3 (m4 a))) -> (a -> m1 (m2 (m3 (m4 b)))) -> m1 (m2 (m3 (m4 b)))

(>====>) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => (a -> m1 (m2 (m3 (m4 b)))) -> (b -> m1 (m2 (m3 (m4 c)))) -> a -> m1 (m2 (m3 (m4 c)))
f >====> g = \x -> f x >>>>== g
(>>>>~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m1 (m2 (m3 (m4 a))) -> m1 (m2 (m3 (m4 b))) -> m1 (m2 (m3 (m4 b)))
m >>>>~ k = m >>>>== \_ -> k


instance Monad4 Maybe where
    mmmmv >>>>== f = 
        mmmmv >>>== \mv ->
        case mv of 
            Nothing -> (***:) Nothing
            Just a  -> f a

instance Monad4 [] where
    mmmmv >>>>== f = 
        mmmmv >>>== \xs -> 
        foldr (\x acc -> f x <<<$|(++)|*>>> acc) ((***:) []) xs 

instance Monad4 (Either e) where
    mmmmv >>>>== f = 
        mmmmv >>>== \mv -> 
        case mv of
            Left l  -> (***:) (Left l)
            Right r -> f r

-- -----------------------------------------------------------------------------
-- Level-5 functions

infixr 1  >=====>
infixr 1  >>>>>~, >>>>>== 
-- TODO: >>>>>~

class (Monad m5) => Monad5 m5 where
  (>>>>>==) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4) => m1 (m2 (m3 (m4 (m5 a)))) -> (a -> m1 (m2 (m3 (m4 (m5 b))))) -> m1 (m2 (m3 (m4 (m5 b))))

(>=====>) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => (a -> m1 (m2 (m3 (m4 (m5 b))))) -> (b -> m1 (m2 (m3 (m4 (m5 c))))) -> a -> m1 (m2 (m3 (m4 (m5 c))))
f >=====> g = \x -> f x >>>>>== g
(>>>>>~) :: (Monad m1, Monad2 m2, Monad3 m3, Monad4 m4, Monad5 m5) => m1 (m2 (m3 (m4 (m5 a)))) -> m1 (m2 (m3 (m4 (m5 b)))) -> m1 (m2 (m3 (m4 (m5 b))))
m >>>>>~ k = m >>>>>== \_ -> k

instance Monad5 Maybe where
    mmmmmv >>>>>== f = 
        mmmmmv >>>>== \mv ->
        case mv of 
            Nothing -> (****:) Nothing
            Just a  -> f a

instance Monad5 [] where
    mmmmmv >>>>>== f = 
        mmmmmv >>>>== \xs -> 
        foldr (\x acc -> f x <<<<$|(++)|*>>>> acc) ((****:) []) xs 

instance Monad5 (Either e) where
    mmmmmv >>>>>== f = 
        mmmmmv >>>>== \mv -> 
        case mv of
            Left l  -> (****:) (Left l)
            Right r -> f r

