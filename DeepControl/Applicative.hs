{-|
Module      : DeepControl.Applicative
Description : Deepened the usual Control.Applicative module.
Copyright   : (c) 2015 KONISHI Yohsuke 
License     : BSD-style (see the LICENSE file in the distribution)
Maintainer  : ocean0yohsuke@gmail.com
Stability   : experimental
Portability : portable

This module enables you to program in applicative style for more __deeper__ level than the usual 'Control.Applicative' module expresses.
You would soon realize exactly what __/more deeper level/__ means by reading the example codes in order, which are attached on the functions below.
-}
module DeepControl.Applicative (
    module Control.Applicative,

    -- * Level-0
    -- ** bra-ket notation
    (|>), (<|),

    -- * Level-1
    -- ** cover notation
    (.*),
    -- ** bra-ket notation
    (|$>), (<$|), (|*>), (<*|),
    -- ** cover-bracket notation
    (|*), (*|),

    -- * Level-2
    -- ** cover notation
    (.**), (-*),
    -- ** bra-ket notation
    (|$>>), (<<$|), (|*>>), (<<*|),
    -- ** cover-bracket notation
    (|**), (**|), (|-*), (|*-), (-*|), (*-|),
    -- ** sequence notation
    (*>>), (<<*), 
    -- ** cover-sequence notation
    (-*>), (<-*), (*->), (<*-), 

    -- * Level-3
    -- ** cover notation
    (.***), 
    (*-*), (-**), 
    (--*),
    -- ** bra-ket notation
    (|$>>>), (<<<$|), (|*>>>), (<<<*|),
    -- ** cover-bracket notation
    (|***), 
    (|-**), (|*-*), (|**-), 
    (|--*), (|-*-), (|*--),
    (***|),
    (-**|), (*-*|), (**-|), 
    (--*|), (-*-|), (*--|),
    -- ** sequence notation
    (*>>>), (<<<*),
    -- ** cover-sequence notation
    (*-->), (-*->), (--*>), 
    (**->), (*-*>), (-**>),
    (<*--), (<-*-), (<--*), 
    (<**-), (<*-*), (<-**),

    -- * Level-4
    -- ** cover notation
    (.****), 
    (---*),
    (--**), (-*-*), (*--*), 
    (-***), (*-**), (**-*),
    -- ** bra-ket notation
    (|$>>>>), (<<<<$|), (|*>>>>), (<<<<*|),
    -- ** cover-bracket notation
    (|****), 
    (|-***), (|*-**), (|**-*), (|***-),  
    (|--**), (|-*-*), (|*--*), (|*-*-), (|-**-), (|**--),
    (|---*), (|--*-), (|-*--), (|*---),
    (****|),
    (-***|), (*-**|), (**-*|), (***-|),  
    (--**|), (-*-*|), (*--*|), (*-*-|), (-**-|), (**--|),
    (---*|), (--*-|), (-*--|), (*---|),
    -- ** sequence notation
    (*>>>>), (<<<<*),
    -- ** cover-sequence notation
    (<---*), (<--*-), (<-*--), (<*---),
    (<--**), (<-*-*), (<*--*), (<*-*-), (<-**-), (<**--),
    (<-***), (<*-**), (<**-*), (<***-),
    (---*>), (--*->), (-*-->), (*--->),
    (--**>), (-*-*>), (*--*>), (*-*->), (-**->), (**-->),
    (-***>), (*-**>), (**-*>), (***->),

    -- * Level-5
    -- ** cover notation
    (.*****), 
    (----*), 
    (---**), (--*-*), (-*--*), (*---*),
    (--***), (-*-**), (*--**), (*-*-*), (-**-*), (**--*),
    (-****), (*-***), (**-**), (***-*),
    -- ** bra-ket notation
    (|$>>>>>), (<<<<<$|), (|*>>>>>), (<<<<<*|),
    -- ** cover-bracket notation
    (|-****), (|*-***), (|**-**), (|***-*), (|****-),
    (|--***), (|-*-**), (|*--**), (|*-*-*), (|-**-*), (|**--*), (|**-*-), (|*-**-), (|-***-), (|***--),
    (|---**), (|--*-*), (|-*--*), (|*---*), (|*--*-), (|-*-*-), (|--**-), (|-**--), (|*-*--), (|**---),
    (|----*), (|---*-), (|--*--), (|-*---), (|*----),
    (-****|), (*-***|), (**-**|), (***-*|), (****-|),
    (--***|), (-*-**|), (*--**|), (*-*-*|), (-**-*|), (**--*|), (**-*-|), (*-**-|), (-***-|), (***--|),
    (---**|), (--*-*|), (-*--*|), (*---*|), (*--*-|), (-*-*-|), (--**-|), (-**--|), (*-*--|), (**---|),
    (----*|), (---*-|), (--*--|), (-*---|), (*----|),
    -- ** sequence notation
    (*>>>>>), (<<<<<*),
    -- ** cover-sequence notation
    (<----*), (<---*-), (<--*--), (<-*---), (<*----),
    (<---**), (<--*-*), (<-*--*), (<*---*), (<*--*-), (<-*-*-), (<--**-), (<-**--), (<*-*--), (<**---),
    (<--***), (<-*-**), (<*--**), (<*-*-*), (<-**-*), (<**--*), (<**-*-), (<*-**-), (<-***-), (<***--),
    (<-****), (<*-***), (<**-**), (<***-*), (<****-),
    (----*>), (---*->), (--*-->), (-*--->), (*---->),
    (---**>), (--*-*>), (-*--*>), (*---*>), (*--*->), (-*-*->), (--**->), (-**-->), (*-*-->), (**--->),
    (--***>), (-*-**>), (*--**>), (*-*-*>), (-**-*>), (**--*>), (**-*->), (*-**->), (-***->), (***-->),
    (-****>), (*-***>), (**-**>), (***-*>), (****->),

    ) where 

import Control.Applicative

-------------------------------------------------------------------------------
-- Level-0 functions

infixl 4  |>, <|

-- | Alias to @'$'@. 
-- 
-- >>> (1+) |> 2
-- 3
(|>) :: (a -> b) -> a -> b
(|>) = ($)

-- | The auguments-flipped function of @'|>'@. 
-- 
-- >>> 1 <| (+2) 
-- 3 
-- >>> 1 <|(+)|> 2 
-- 3 
-- >>> 1 <|(+)|> 2 <|(*)|> 3
-- 9
--
-- >>> 1 <|(,)|> 2
-- (1,2)
(<|) :: a -> (a -> b) -> b
(<|) = flip (|>)

-------------------------------------------------------------------------------
-- Level-1 functions

infixl 6  .*
-- | Alias to @'pure'@.
(.*) :: (Applicative f) => a -> f a
(.*) = pure

infixl 4 |$>
-- | Alias to @'<$>'@.
--
-- >>> (1+) |$> [2] 
-- [3]
(|$>) :: Functor f => (a -> b) -> f a -> f b
(|$>) = (<$>)

infixl 3  <$|, |*>, <*|, |*, *|

-- | The auguments-flipped function of @'|$>'@.
--
-- >>> [1] <$| (+2) 
-- [3]
--
-- >>> ("<"++)|$> ["a","b"] <$|(++">")
-- ["<a>","<b>"]
(<$|) :: Functor f => f a -> (a -> b) -> f b
(<$|) = flip (|$>)

-- | Alias to @'<*>'@.
-- 
-- >>> [(1+)] |*> [2]
-- [3]
--
-- >>> [1] <$|(+)|*> [2]
-- [3]
-- >>> [1] <$|(+)|*> [0,1,2] 
-- [1,2,3]
-- >>> [0,1] <$|(+)|*> [2,3] <$|(^)|*> [4,5]
-- [16,32,81,243,81,243,256,1024]
--
-- >>> foldr (\x acc -> x <$|(:)|*> acc) ((.*) []) [Just 1, Just 2,  Just 3]
-- Just [1,2,3]
-- >>> foldr (\x acc -> x <$|(:)|*> acc) ((.*) []) [Just 1, Nothing, Just 3]
-- Nothing
--
-- >>> filter (even <$|(&&)|*> (10 >)) [1..100]
-- [2,4,6,8]
-- >>> filter (even <$|(&&)|*> (10 >) <$|(&&)|*> (5 <)) [1..100]
-- [6,8]
(|*>) :: Applicative f => f (a -> b) -> f a -> f b
(|*>) = (<*>)

-- | The auguments-flipped function of @'|*>'@. 
(<*|) :: Applicative f => f a -> f (a -> b) -> f b
(<*|) = flip (|*>)

-- | Definition: @ f |* x = f |*> (.*) x @
--
-- >>> [(1+)] |* 2
-- [3]
-- >>> [1] <$|(+)|* 2 
-- [3]
--
-- >>> (,) |$> ["a1","a2"] |* 'b'
-- [("a1",'b'),("a2",'b')]
--
-- >>> (,,) 'a' |$> ["b1","b2"] |* 'c'
-- [('a',"b1",'c'),('a',"b2",'c')]
--
-- >>> (,,,) 'a' |$> ["b1","b2"] |* 'c' |* 'd'
-- [('a',"b1",'c','d'),('a',"b2",'c','d')]
-- >>> (,,,) 'a' |$> ["b1","b2"] |* 'c' |*> ["d1","d2"]
-- [('a',"b1",'c',"d1"),('a',"b1",'c',"d2"),('a',"b2",'c',"d1"),('a',"b2",'c',"d2")]
--
(|*) :: Applicative f => f (a -> b) -> a -> f b
f |* x = f |*> (.*) x

-- | Definition: @ (*|) = (.*) x <*| f @
--
-- >>> 1 *| [(+2)]
-- [3]
-- >>> 1 *| [(+)] |* 2
-- [3]
-- >>> 1 *|[(+),(-),(*),(^)]|* 2
-- [3,-1,2,1]
-- 
-- >>> 1 *|Just (,)|* 2
-- Just (1,2)
(*|) :: Applicative f => a -> f (a -> b) -> f b
x *| f = (.*) x <*| f

-------------------------------------------------------------------------------
-- Level-2 functions

infixl 6  .**
infixl 6  -*
-- | Definition: @ (.**) = (.*) . (.*) @
--
-- >>> (.**) 1 :: Maybe [Int]
-- Just [1]
(.**) :: (Applicative f1, Applicative f2) => a -> f1 (f2 a)
(.**) = (.*) . (.*)
-- | Definition: @ (-*) = ((.*)|$>)  @
--
-- >>> (-*) (Just 1) :: Maybe [Int]
-- Just [1]
(-*) :: (Applicative f1, Applicative f2) => f1 a -> f1 (f2 a)
(-*) = ((.*)|$>) 

infixl 4  |$>>
-- | Definition: @ (|$>>) = (|$>) . (|$>) @
--
-- >>> (+1) |$>> [[2]]
-- [[3]]
(|$>>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(|$>>) = (|$>) . (|$>)

infixl 3  <<$|, |*>>, <<*|
infixl 3  |**, **|
infixl 3  |-*, |*-, -*|, *-|

-- | The auguments-flipped function of @'|$>>'@
--
-- >>> [[2]] <<$| (+1)
-- [[3]]
(<<$|) :: (Functor f1, Functor f2) => f1 (f2 a) -> (a -> b) -> f1 (f2 b)
(<<$|) = flip (|$>>)

-- | Definition: @ f |*>> x = f <$|(|*>)|*> x @
--
-- >>> [Just 1] <<$|(+)|*>> [Just 2] 
-- [Just 3]
--
-- >>> [Just 1] <<$|(,)|*>> [Just 2]
-- [Just (1,2)]
--
-- >>> [[1]] <<$|(+)|*>> [[2]] <<$|(-)|*>> [[3]]
-- [[0]]
--
-- >>> foldr (\n acc -> n <<$|(+)|*>> acc) ((.**) 0) [Right (Just 1), Right (Just 2), Right (Just 3)] :: Either () (Maybe Int)
-- Right (Just 6)
-- >>> foldr (\n acc -> n <<$|(+)|*>> acc) ((.**) 0) [Right (Just 1), Right Nothing, Right (Just 3)] :: Either () (Maybe Int)
-- Right Nothing
-- >>> foldr (\n acc -> n <<$|(+)|*>> acc) ((.**) 0) [Right (Just 1), Right Nothing, Left ()]
-- Left ()
(|*>>) :: (Applicative f1, Applicative f2) => f1 (f2 (a -> b)) -> f1 (f2 a) -> f1 (f2 b)
-- (|*>>) = liftA2 (|*>)
f |*>> x = f <$|(|*>)|*> x

-- | Definition: @ (<<*|) = liftA2 (<*|) @
(<<*|) :: (Applicative f1, Applicative f2) => f1 (f2 a) -> f1 (f2 (a -> b)) -> f1 (f2 b)
x <<*| f = x <$|(<*|)|*> f

-- | Definition: @ f |** x = f |*>> (.**) x @
--
-- >>> [Just 1] <<$|(+)|** 2
-- [Just 3]
(|**) :: (Applicative f1, Applicative f2) => f1 (f2 (a -> b)) -> a -> f1 (f2 b)
f |** x = f |*>> (.**) x

-- | Definition: @ x **| f = (.**) x <<*| f @
--
-- >>> 1 **|(+)|$>> [Just 2]
-- [Just 3]
--
-- >>> 1 **|[Just (+)]|**  2
-- [Just 3]
-- >>> 1 **|[Just (+), Just (-), Just (*), Nothing]|** 2
-- [Just 3,Just (-1),Just 2,Nothing]
(**|) :: (Applicative f1, Applicative f2) => a -> f1 (f2 (a -> b)) -> f1 (f2 b)
x **| f = (.**) x <<*| f

-- | Definition: @ f |-* x = f |*>> (-*) x @
--
-- >>> [Just 1] <<$|(+)|-* [2]
-- [Just 3]
(|-*) :: (Applicative f1, Applicative f2) => f1 (f2 (a -> b)) -> f1 a -> f1 (f2 b)
f |-* x = f |*>> (-*) x

-- | Definition: @ f |*- x = f |*>> (.*) x @
--
-- >>> [Just 1] <<$|(+)|*- Just 2 
-- [Just 3]
(|*-) :: (Applicative f1, Applicative f2) => f1 (f2 (a -> b)) -> f2 a -> f1 (f2 b)
f |*- x = f |*>> (.*) x

-- | Definition: @ x -*| f = (-*) x <<*| f @
--
-- >>> [1] -*|(+)|$>> [Just 2]
-- [Just 3]
(-*|) :: (Applicative f1, Applicative f2) => f1 a -> f1 (f2 (a -> b)) -> f1 (f2 b)
x -*| f = (-*) x <<*| f

-- | Definition: @ x *-| f = (.*) x <<*| f @
--
-- >>> Just 1 *-|(+)|$>> [Just 2]
-- [Just 3]
-- >>> Just 1 *-|[Just (+)]|** 2
-- [Just 3]
-- >>> Just 1 *-|[Just (+)]|*- Just 2
-- [Just 3]
-- >>> [1] -*|[Just (+)]|*- Just 2
-- [Just 3]
-- >>> [1] -*|[Just (+), Just (-), Just (*), Nothing]|*- Just 2
-- [Just 3,Just (-1),Just 2,Nothing]
-- >>> [1,2] -*|[Just (+), Just (-), Just (*), Nothing]|*- Just 2
-- [Just 3,Just (-1),Just 2,Nothing,Just 4,Just 0,Just 4,Nothing]
--
-- >>> print 1 -*|return [\_ _ -> 3]|-* print 2
-- 1
-- 2
-- [3]
(*-|) :: (Applicative f1, Applicative f2) => f2 a -> f1 (f2 (a -> b)) -> f1 (f2 b)
x *-| f = (.*) x <<*| f

infixl 5  <<*, *>>
infixl 5  *->, <*-, -*>, <-*

-- | Definition: @ f *>> x = f <$|(*>)|*> x @
--
-- >>> sequence $ Just (print 1) *>> (.**) 2
-- 1
-- Just 2
--
-- >>> (-*) (print 1) *>> return (Just 2)
-- 1
-- Just 2
(*>>) :: (Applicative f1, Applicative f2) => f1 (f2 a) -> f1 (f2 b) -> f1 (f2 b)
f *>> x = f <$|(*>)|*> x

-- | Definition: @ (<<*) = liftA2 (<*) @
--
-- >>> sequence $ (.**) 2 <<* Just (print 1)
-- 1
-- Just 2
-- >>> sequence $ Just (print 1) *>> (.**) 3 <<* Just (print 2)
-- 1
-- 2
-- Just 3
--
-- >>> sequence $ [putStr "1", putStr "2"] *>> (.**) 0 <<* [putStr "3", putStr "4"]
-- 13142324[0,0,0,0]
(<<*) :: (Applicative f1, Applicative f2) => f1 (f2 a) -> f1 (f2 b) -> f1 (f2 a)
x <<* f = x <$|(<*)|*> f

-- | Definition: @ a -*> x = (-*) a *>> x @
--
-- >>> print 1 -*> (.*) (Just 2)
-- 1
-- Just 2
(-*>) :: (Applicative f1, Applicative f2) => f1 a -> f1 (f2 b) -> f1 (f2 b)
a -*> x = (-*) a *>> x

-- | Definition: @ x <-* a = x <<* (-*) a @
--
-- >>> (.*) (Just 2) <-* print 1
-- 1
-- Just 2
-- >>> print 1 -*> (.*) (Just 3) <-* print 2
-- 1
-- 2
-- Just 3
-- >>> print 1 -*> (.*) (Just 3) <*- Nothing
-- 1
-- Nothing
(<-*) :: (Applicative f1, Applicative f2) => f1 (f2 b) -> f1 a -> f1 (f2 b)
x <-* a = x <<* (-*) a

-- | Definition: @ a *-> x = (.*) a *>> x @
--
-- >>> sequence $ print 1 *-> Just ((.*) 2)
-- 1
-- Just 2
(*->) :: (Applicative f1, Applicative f2) => f2 a -> f1 (f2 b) -> f1 (f2 b)
a *-> x = (.*) a *>> x

-- | Definition: @ x <*- a = x <<* (.*) a @
--
-- >>> sequence $ Just ((.*) 2) <*- print 1
-- 1
-- Just 2
-- >>> sequence $ print 1 *-> Just ((.*) 3) <*- print 2
-- 1
-- 2
-- Just 3
-- >>> sequence $ print 1 *-> Just ((.*) 3) <-* Nothing
-- Nothing
(<*-) :: (Applicative f1, Applicative f2) => f1 (f2 b) -> f2 a -> f1 (f2 b)
x <*- a = x <<* (.*) a

-------------------------------------------------------------------------------
-- Level-3 functions

infixl 6  .***
(.***) :: (Applicative f1, Applicative f2, Applicative f3) => a -> f1 (f2 (f3 a))
(.***) = (.*) . (.**)
infixl 6  -**, *-*, --*
(--*) :: (Applicative f1, Applicative f2, Applicative f3) => f1 (f2 a) -> f1 (f2 (f3 a))
(--*) = ((-*)|$>)
(-**) :: (Applicative f1, Applicative f2, Applicative f3) => f1 a -> f1 (f2 (f3 a))
(-**) = ((.**)|$>) 
(*-*) :: (Applicative f1, Applicative f2, Applicative f3) => f2 a -> f1 (f2 (f3 a))
(*-*) = (--*) . (.*) 

infixl 4  |$>>>
(|$>>>) :: (Functor f1, Functor f2, Functor f3) => (a -> b) -> f1 (f2 (f3 a)) -> f1 (f2 (f3 b))
(|$>>>) = (|$>) . (|$>>)

infixl 3  <<<$|, |*>>>, <<<*|
(<<<$|) :: (Functor f1, Functor f2, Functor f3) => f1 (f2 (f3 a)) -> (a -> b) -> f1 (f2 (f3 b))
(<<<$|) = flip (|$>>>)
(|*>>>) :: (Applicative f1, Applicative f2, Applicative f3) => f1 (f2 (f3 (a -> b))) -> f1 (f2 (f3 a)) -> f1 (f2 (f3 b))
f |*>>> x = f <$|(|*>>)|*> x
(<<<*|) :: (Applicative f1, Applicative f2, Applicative f3) => f1 (f2 (f3 a)) -> f1 (f2 (f3 (a -> b))) -> f1 (f2 (f3 b))
x <<<*| f = x <$|(<<*|)|*> f

infixl 3  |***, ***|
(|***) :: (Applicative f1, Applicative f2, Applicative f3) => f1 (f2 (f3 (a -> b))) -> a -> f1 (f2 (f3 b))
f |*** x = f |*>>> (.***) x
(***|) :: (Applicative f1, Applicative f2, Applicative f3) => a -> f1 (f2 (f3 (a -> b))) -> f1 (f2 (f3 b))
x ***| f = (.***) x <<<*| f

infixl 3  |-**, |*-*, |**-, |--*, |-*-, |*--
(|-**) :: (Applicative f1, Applicative f2, Applicative f3) => f1 (f2 (f3 (a -> b))) -> f1 a -> f1 (f2 (f3 b))
f |-** x = f |*>>> (-**) x
(|*-*) :: (Applicative f1, Applicative f2, Applicative f3) => f1 (f2 (f3 (a -> b))) -> f2 a -> f1 (f2 (f3 b))
f |*-* x = f |*>>> (*-*) x
(|**-) :: (Applicative f1, Applicative f2, Applicative f3) => f1 (f2 (f3 (a -> b))) -> f3 a -> f1 (f2 (f3 b))
f |**- x = f |*>>> (.**) x
(|--*) :: (Applicative f1, Applicative f2, Applicative f3) => f1 (f2 (f3 (a -> b))) -> f1 (f2 a) -> f1 (f2 (f3 b))
f |--* x = f |*>>> (--*) x
(|*--) :: (Applicative f1, Applicative f2, Applicative f3) => f1 (f2 (f3 (a -> b))) -> f2 (f3 a) -> f1 (f2 (f3 b))
f |*-- x = f |*>>> (.*) x
(|-*-) :: (Applicative f1, Applicative f2, Applicative f3) => f1 (f2 (f3 (a -> b))) -> f1 (f3 a) -> f1 (f2 (f3 b))
f |-*- x = f |*>>> (-*) x

infixl 3  --*|, -*-|, *--|, -**|, *-*|, **-|
(--*|) :: (Applicative f1, Applicative f2, Applicative f3) => f1 (f2 a) -> f1 (f2 (f3 (a -> b))) -> f1 (f2 (f3 b))
x --*| f = (--*) x <<<*| f
(-*-|) :: (Applicative f1, Applicative f2, Applicative f3) => f1 (f3 a) -> f1 (f2 (f3 (a -> b))) -> f1 (f2 (f3 b))
x -*-| f = (-*) x <<<*| f
(*--|) :: (Applicative f1, Applicative f2, Applicative f3) => f2 (f3 a) -> f1 (f2 (f3 (a -> b))) -> f1 (f2 (f3 b))
x *--| f = (.*) x <<<*| f
(-**|) :: (Applicative f1, Applicative f2, Applicative f3) => f1 a -> f1 (f2 (f3 (a -> b))) -> f1 (f2 (f3 b))
x -**| f = (-**) x <<<*| f
(*-*|) :: (Applicative f1, Applicative f2, Applicative f3) => f2 a -> f1 (f2 (f3 (a -> b))) -> f1 (f2 (f3 b))
x *-*| f = (*-*) x <<<*| f
(**-|) :: (Applicative f1, Applicative f2, Applicative f3) => f3 a -> f1 (f2 (f3 (a -> b))) -> f1 (f2 (f3 b))
x **-| f = (.**) x <<<*| f

infixl 5  <<<*, *>>>
(*>>>) :: (Applicative f1, Applicative f2, Applicative f3) => f1 (f2 (f3 a)) -> f1 (f2 (f3 b)) -> f1 (f2 (f3 b))
f *>>> x = f <$|(*>>)|*> x
(<<<*) :: (Applicative f1, Applicative f2, Applicative f3) => f1 (f2 (f3 a)) -> f1 (f2 (f3 b)) -> f1 (f2 (f3 a))
x <<<* f = x <$|(<<*)|*> f

infixl 5  *-->, -*->, --*>, **->, *-*>, -**>
(*-->) :: (Applicative f1, Applicative f2, Applicative f3) => f2 (f3 a) -> f1 (f2 (f3 b)) -> f1 (f2 (f3 b))
a *--> x = (.*) a *>>> x
(-*->) :: (Applicative f1, Applicative f2, Applicative f3) => f1 (f3 a) -> f1 (f2 (f3 b)) -> f1 (f2 (f3 b))
a -*-> x = (-*) a *>>> x
(--*>) :: (Applicative f1, Applicative f2, Applicative f3) => f1 (f2 a) -> f1 (f2 (f3 b)) -> f1 (f2 (f3 b))
a --*> x = (--*) a *>>> x
(**->) :: (Applicative f1, Applicative f2, Applicative f3) => f3 a -> f1 (f2 (f3 b)) -> f1 (f2 (f3 b))
a **-> x = (.**) a *>>> x
(*-*>) :: (Applicative f1, Applicative f2, Applicative f3) => f2 a -> f1 (f2 (f3 b)) -> f1 (f2 (f3 b))
a *-*> x = (*-*) a *>>> x
(-**>) :: (Applicative f1, Applicative f2, Applicative f3) => f1 a -> f1 (f2 (f3 b)) -> f1 (f2 (f3 b))
a -**> x = (-**) a *>>> x

infixl 5  <*--, <-*-, <--*, <**-, <*-*, <-**
(<*--) :: (Applicative f1, Applicative f2, Applicative f3) => f1 (f2 (f3 b)) -> f2 (f3 a) -> f1 (f2 (f3 b))
x <*-- a = x <<<* (.*) a
(<-*-) :: (Applicative f1, Applicative f2, Applicative f3) => f1 (f2 (f3 b)) -> f1 (f3 a) -> f1 (f2 (f3 b))
x <-*- a = x <<<* (-*) a
(<--*) :: (Applicative f1, Applicative f2, Applicative f3) => f1 (f2 (f3 b)) -> f1 (f2 a) -> f1 (f2 (f3 b))
x <--* a = x <<<* (--*) a
(<**-) :: (Applicative f1, Applicative f2, Applicative f3) => f1 (f2 (f3 b)) -> f3 a -> f1 (f2 (f3 b))
x <**- a = x <<<* (.**) a 
(<*-*) :: (Applicative f1, Applicative f2, Applicative f3) => f1 (f2 (f3 b)) -> f2 a -> f1 (f2 (f3 b))
x <*-* a = x <<<* (*-*) a
(<-**) :: (Applicative f1, Applicative f2, Applicative f3) => f1 (f2 (f3 b)) -> f1 a -> f1 (f2 (f3 b))
x <-** a = x <<<* (-**) a

--------------------------------------------------------------------------------
-- Level-4 functions

infixl 6  .****
(.****) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => a -> f1 (f2 (f3 (f4 a)))
(.****) = (.***) . (.*)
infixl 6  ---*
(---*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 a)) -> f1 (f2 (f3 (f4 a)))
(---*) = ((--*)|$>)
infixl 6  --**, -*-*, *--*
(--**) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 a) -> f1 (f2 (f3 (f4 a)))
(--**) = (---*) . (--*) 
(-*-*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f3 a) -> f1 (f2 (f3 (f4 a)))
(-*-*) = (---*) . (-*)
(*--*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f2 (f3 a) -> f1 (f2 (f3 (f4 a)))
(*--*) = (---*) . (.*)
infixl 6  -***, *-**, **-*
(-***) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 a -> f1 (f2 (f3 (f4 a)))
(-***) = ((.***)|$>) 
(*-**) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f2 a -> f1 (f2 (f3 (f4 a)))
(*-**) = (---*) . (*-*) 
(**-*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f3 a -> f1 (f2 (f3 (f4 a)))
(**-*) = (---*) . (.**)

infixl 4  |$>>>>
(|$>>>>) :: (Functor f1, Functor f2, Functor f3, Functor f4) => (a -> b) -> f1 (f2 (f3 (f4 a))) -> f1 (f2 (f3 (f4 b)))
(|$>>>>) = (|$>) . (|$>>>)

infixl 3  <<<<$|, |*>>>>, <<<<*|
(<<<<$|) :: (Functor f1, Functor f2, Functor f3, Functor f4) => f1 (f2 (f3 (f4 a))) -> (a -> b) -> f1 (f2 (f3 (f4 b)))
(<<<<$|) = flip (|$>>>>)
(|*>>>>) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 (a -> b)))) -> f1 (f2 (f3 (f4 a))) -> f1 (f2 (f3 (f4 b)))
f |*>>>> x = f <$|(|*>>>)|*> x
(<<<<*|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 a))) -> f1 (f2 (f3 (f4 (a -> b)))) -> f1 (f2 (f3 (f4 b)))
x <<<<*| f = x <$|(<<<*|)|*> f

infixl 3  |****
(|****) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 (a -> b)))) -> a -> f1 (f2 (f3 (f4 b)))
f |**** x = f |*>>>> (.****) x
infixl 3  |-***, |*-**, |**-*, |***-  
(|-***) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 (a -> b)))) -> f1 a -> f1 (f2 (f3 (f4 b)))
f |-*** x = f |*>>>> (-***) x
(|*-**) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 (a -> b)))) -> f2 a -> f1 (f2 (f3 (f4 b)))
f |*-** x = f |*>>>> (*-**) x
(|**-*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 (a -> b)))) -> f3 a -> f1 (f2 (f3 (f4 b)))
f |**-* x = f |*>>>> (**-*) x
(|***-) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 (a -> b)))) -> f4 a -> f1 (f2 (f3 (f4 b)))
f |***- x = f |*>>>> (.***) x
infixl 3  |--**, |-*-*, |*--*, |*-*-, |-**-, |**--
(|--**) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 (a -> b)))) -> f1 (f2 a) -> f1 (f2 (f3 (f4 b)))
f |--** x = f |*>>>> (--**) x
(|-*-*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 (a -> b)))) -> f1 (f3 a) -> f1 (f2 (f3 (f4 b)))
f |-*-* x = f |*>>>> (-*-*) x
(|*--*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 (a -> b)))) -> f2 (f3 a) -> f1 (f2 (f3 (f4 b)))
f |*--* x = f |*>>>> (*--*) x
(|*-*-) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 (a -> b)))) -> f2 (f4 a) -> f1 (f2 (f3 (f4 b)))
f |*-*- x = f |*>>>> (*-*) x
(|-**-) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 (a -> b)))) -> f1 (f4 a) -> f1 (f2 (f3 (f4 b)))
f |-**- x = f |*>>>> (-**) x
(|**--) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 (a -> b)))) -> f3 (f4 a) -> f1 (f2 (f3 (f4 b)))
f |**-- x = f |*>>>> (.**) x
infixl 3  |---*, |--*-, |-*--, |*---
(|---*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 (a -> b)))) -> f1 (f2 (f3 a)) -> f1 (f2 (f3 (f4 b)))
f |---* x = f |*>>>> (---*) x
(|--*-) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 (a -> b)))) -> f1 (f2 (f4 a)) -> f1 (f2 (f3 (f4 b)))
f |--*- x = f |*>>>> (--*) x
(|-*--) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 (a -> b)))) -> f1 (f3 (f4 a)) -> f1 (f2 (f3 (f4 b)))
f |-*-- x = f |*>>>> (-*) x
(|*---) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 (a -> b)))) -> f2 (f3 (f4 a)) -> f1 (f2 (f3 (f4 b)))
f |*--- x = f |*>>>> (.*) x

infixl 3  ****|
(****|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => a -> f1 (f2 (f3 (f4 (a -> b)))) -> f1 (f2 (f3 (f4 b)))
x ****| f = (.****) x <<<<*| f
infixl 3  -***|, *-**|, **-*|, ***-|  

(-***|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 a -> f1 (f2 (f3 (f4 (a -> b)))) -> f1 (f2 (f3 (f4 b)))
x -***| f = (-***) x <<<<*| f
(*-**|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f2 a -> f1 (f2 (f3 (f4 (a -> b)))) -> f1 (f2 (f3 (f4 b)))
x *-**| f = (*-**) x <<<<*| f
(**-*|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f3 a -> f1 (f2 (f3 (f4 (a -> b)))) -> f1 (f2 (f3 (f4 b)))
x **-*| f = (**-*) x <<<<*| f
(***-|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f4 a -> f1 (f2 (f3 (f4 (a -> b)))) -> f1 (f2 (f3 (f4 b)))
x ***-| f = (.***) x <<<<*| f
infixl 3  --**|, -*-*|, *--*|, *-*-|, -**-|, **--|
(--**|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 a) -> f1 (f2 (f3 (f4 (a -> b)))) -> f1 (f2 (f3 (f4 b)))
x --**| f = (--**) x <<<<*| f
(-*-*|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f3 a) -> f1 (f2 (f3 (f4 (a -> b)))) -> f1 (f2 (f3 (f4 b)))
x -*-*| f = (-*-*) x <<<<*| f
(*--*|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f2 (f3 a) -> f1 (f2 (f3 (f4 (a -> b)))) -> f1 (f2 (f3 (f4 b)))
x *--*| f = (*--*) x <<<<*| f
(*-*-|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f2 (f4 a) -> f1 (f2 (f3 (f4 (a -> b)))) -> f1 (f2 (f3 (f4 b)))
x *-*-| f = (*-*) x <<<<*| f
(-**-|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f4 a) -> f1 (f2 (f3 (f4 (a -> b)))) -> f1 (f2 (f3 (f4 b)))
x -**-| f = (-**) x <<<<*| f
(**--|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f3 (f4 a) -> f1 (f2 (f3 (f4 (a -> b)))) -> f1 (f2 (f3 (f4 b)))
x **--| f = (.**) x <<<<*| f
infixl 3  ---*|, --*-|, -*--|, *---|
(---*|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 a)) -> f1 (f2 (f3 (f4 (a -> b)))) -> f1 (f2 (f3 (f4 b)))
x ---*| f = (---*) x <<<<*| f
(--*-|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f4 a)) -> f1 (f2 (f3 (f4 (a -> b)))) -> f1 (f2 (f3 (f4 b)))
x --*-| f = (--*) x <<<<*| f
(-*--|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f3 (f4 a)) -> f1 (f2 (f3 (f4 (a -> b)))) -> f1 (f2 (f3 (f4 b)))
x -*--| f = (-*) x <<<<*| f
(*---|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f2 (f3 (f4 a)) -> f1 (f2 (f3 (f4 (a -> b)))) -> f1 (f2 (f3 (f4 b)))
x *---| f = (.*) x <<<<*| f

infixl 5  <<<<*, *>>>>
(*>>>>) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 a))) -> f1 (f2 (f3 (f4 b))) -> f1 (f2 (f3 (f4 b)))
f *>>>> x = f <$|(*>>>)|*> x
(<<<<*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 a))) -> f1 (f2 (f3 (f4 b))) -> f1 (f2 (f3 (f4 a)))
x <<<<* f = x <$|(<<<*)|*> f

infixl 5  <---*, <--*-, <-*--, <*---
(<---*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 b))) -> f1 (f2 (f3 a)) -> f1 (f2 (f3 (f4 b)))
x <---* a = x <<<<* (---*) a
(<--*-) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 b))) -> f1 (f2 (f4 a)) -> f1 (f2 (f3 (f4 b)))
x <--*- a = x <<<<* (--*) a
(<-*--) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 b))) -> f1 (f3 (f4 a)) -> f1 (f2 (f3 (f4 b)))
x <-*-- a = x <<<<* (-*) a
(<*---) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 b))) -> f2 (f3 (f4 a)) -> f1 (f2 (f3 (f4 b)))
x <*--- a = x <<<<* (.*) a
infixl 5  <--**, <-*-*, <*--*, <*-*-, <-**-, <**--
(<--**) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 b))) -> f1 (f2 a) -> f1 (f2 (f3 (f4 b)))
x <--** a = x <<<<* (--**) a
(<-*-*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 b))) -> f1 (f3 a) -> f1 (f2 (f3 (f4 b)))
x <-*-* a = x <<<<* (-*-*) a
(<*--*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 b))) -> f2 (f3 a) -> f1 (f2 (f3 (f4 b)))
x <*--* a = x <<<<* (*--*) a
(<*-*-) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 b))) -> f2 (f4 a) -> f1 (f2 (f3 (f4 b)))
x <*-*- a = x <<<<* (*-*) a
(<-**-) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 b))) -> f1 (f4 a) -> f1 (f2 (f3 (f4 b)))
x <-**- a = x <<<<* (-**) a
(<**--) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 b))) -> f3 (f4 a) -> f1 (f2 (f3 (f4 b)))
x <**-- a = x <<<<* (.**) a
infixl 5  <-***, <*-**, <**-*, <***-
(<-***) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 b))) -> f1 a -> f1 (f2 (f3 (f4 b)))
x <-*** a = x <<<<* (-***) a
(<*-**) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 b))) -> f2 a -> f1 (f2 (f3 (f4 b)))
x <*-** a = x <<<<* (*-**) a
(<**-*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 b))) -> f3 a -> f1 (f2 (f3 (f4 b)))
x <**-* a = x <<<<* (**-*) a
(<***-) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 b))) -> f4 a -> f1 (f2 (f3 (f4 b)))
x <***- a = x <<<<* (.***) a

infixl 5  ---*>, --*->, -*-->, *--->
(---*>) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 a)) -> f1 (f2 (f3 (f4 b))) -> f1 (f2 (f3 (f4 b)))
a ---*> x = (---*) a *>>>> x 
(--*->) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f4 a)) -> f1 (f2 (f3 (f4 b))) -> f1 (f2 (f3 (f4 b)))
a --*-> x = (--*) a *>>>> x
(-*-->) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f3 (f4 a)) -> f1 (f2 (f3 (f4 b))) -> f1 (f2 (f3 (f4 b)))
a -*--> x = (-*) a *>>>> x
(*--->) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f2 (f3 (f4 a)) -> f1 (f2 (f3 (f4 b))) -> f1 (f2 (f3 (f4 b)))
a *---> x = (.*) a *>>>> x
infixl 5  --**>, -*-*>, *--*>, *-*->, -**->, **-->
(--**>) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 a) -> f1 (f2 (f3 (f4 b))) -> f1 (f2 (f3 (f4 b)))
a --**> x = (--**) a *>>>> x
(-*-*>) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f3 a) -> f1 (f2 (f3 (f4 b))) -> f1 (f2 (f3 (f4 b)))
a -*-*> x = (-*-*) a *>>>> x
(*--*>) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f2 (f3 a) -> f1 (f2 (f3 (f4 b))) -> f1 (f2 (f3 (f4 b)))
a *--*> x = (*--*) a *>>>> x
(*-*->) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f2 (f4 a) -> f1 (f2 (f3 (f4 b))) -> f1 (f2 (f3 (f4 b)))
a *-*-> x = (*-*) a *>>>> x
(-**->) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f4 a) -> f1 (f2 (f3 (f4 b))) -> f1 (f2 (f3 (f4 b)))
a -**-> x = (-**) a *>>>> x
(**-->) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f3 (f4 a) -> f1 (f2 (f3 (f4 b))) -> f1 (f2 (f3 (f4 b)))
a **--> x = (.**) a *>>>> x
infixl 5  -***>, *-**>, **-*>, ***->
(-***>) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 a -> f1 (f2 (f3 (f4 b))) -> f1 (f2 (f3 (f4 b)))
a -***> x = (-***) a *>>>> x
(*-**>) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f2 a -> f1 (f2 (f3 (f4 b))) -> f1 (f2 (f3 (f4 b)))
a *-**> x = (*-**) a *>>>> x
(**-*>) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f3 a -> f1 (f2 (f3 (f4 b))) -> f1 (f2 (f3 (f4 b)))
a **-*> x = (**-*) a *>>>> x
(***->) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f4 a -> f1 (f2 (f3 (f4 b))) -> f1 (f2 (f3 (f4 b)))
a ***-> x = (.***) a *>>>> x

-------------------------------------------------------------------------------
-- Level-5 functions

infixl 6  .*****
(.*****) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => a -> f1 (f2 (f3 (f4 (f5 a))))
(.*****) = (.*) . (.****)
infixl 6  ----*
(----*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 a))) -> f1 (f2 (f3 (f4 (f5 a))))
(----*) = ((---*)|$>)
infixl 6  ---**, --*-*, -*--*, *---*
(---**) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 a)) -> f1 (f2 (f3 (f4 (f5 a))))
(---**) = ((.**)|$>>>)
(--*-*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f4 a)) -> f1 (f2 (f3 (f4 (f5 a))))
(--*-*) = (----*) . (--*)
(-*--*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f3 (f4 a)) -> f1 (f2 (f3 (f4 (f5 a))))
(-*--*) = (----*) . (-*)
(*---*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f2 (f3 (f4 a)) -> f1 (f2 (f3 (f4 (f5 a))))
(*---*) = (----*) . (.*)
infixl 6  --***, -*-**, *--**, *-*-*, -**-*, **--*
(--***) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 a) -> f1 (f2 (f3 (f4 (f5 a))))
(--***) = ((-***)|$>)
(-*-**) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f3 a) -> f1 (f2 (f3 (f4 (f5 a))))
(-*-**) = (---**) . (-*)
(*--**) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f2 (f3 a) -> f1 (f2 (f3 (f4 (f5 a))))
(*--**) = (---**) . (.*)
(*-*-*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f2 (f4 a) -> f1 (f2 (f3 (f4 (f5 a))))
(*-*-*) = (----*) . (*-*)
(-**-*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f4 a) -> f1 (f2 (f3 (f4 (f5 a))))
(-**-*) = (----*) . (-**)
(**--*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f3 (f4 a) -> f1 (f2 (f3 (f4 (f5 a))))
(**--*) = (----*) . (.**)
infixl 6  -****, *-***, **-**, ***-*
(-****) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 a -> f1 (f2 (f3 (f4 (f5 a))))
(-****) = ((.****)|$>)
(*-***) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f2 a -> f1 (f2 (f3 (f4 (f5 a))))
(*-***) = (--***) . (.*)
(**-**) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f3 a -> f1 (f2 (f3 (f4 (f5 a))))
(**-**) = (---**) . (.**)
(***-*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f4 a -> f1 (f2 (f3 (f4 (f5 a))))
(***-*) = (----*) . (.***)

infixl 4  |$>>>>>
(|$>>>>>) :: (Functor f1, Functor f2, Functor f3, Functor f4, Functor f5) => (a -> b) -> f1 (f2 (f3 (f4 (f5 a)))) -> f1 (f2 (f3 (f4 (f5 b))))
(|$>>>>>) = (|$>) . (|$>>>>)
infixl 3  <<<<<$|, |*>>>>>, <<<<<*|
(<<<<<$|) :: (Functor f1, Functor f2, Functor f3, Functor f4, Functor f5) => f1 (f2 (f3 (f4 (f5 a)))) -> (a -> b) -> f1 (f2 (f3 (f4 (f5 b))))
(<<<<<$|) = flip (|$>>>>>)
(|*>>>>>) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f3 (f4 (f5 a)))) -> f1 (f2 (f3 (f4 (f5 b))))
f |*>>>>> x = f <$|(|*>>>>)|*> x
(<<<<<*|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 a)))) -> f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f3 (f4 (f5 b))))
x <<<<<*| f = x <$|(<<<<*|)|*> f

infixl 3  |-****, |*-***, |**-**, |***-*, |****-
(|-****) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 a -> f1 (f2 (f3 (f4 (f5 b))))
f |-**** x = f |*>>>>> (-****) x
(|*-***) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f2 a -> f1 (f2 (f3 (f4 (f5 b))))
f |*-*** x = f |*>>>>> (*-***) x
(|**-**) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f3 a -> f1 (f2 (f3 (f4 (f5 b))))
f |**-** x = f |*>>>>> (**-**) x
(|***-*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f4 a -> f1 (f2 (f3 (f4 (f5 b))))
f |***-* x = f |*>>>>> (***-*) x
(|****-) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f5 a -> f1 (f2 (f3 (f4 (f5 b))))
f |****- x = f |*>>>>> (.****) x
infixl 3  |--***, |-*-**, |*--**, |*-*-*, |-**-*, |**--*, |**-*-, |*-**-, |-***-, |***--
(|--***) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 a) -> f1 (f2 (f3 (f4 (f5 b))))
f |--*** x = f |*>>>>> (--***) x
(|-*-**) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f3 a) -> f1 (f2 (f3 (f4 (f5 b))))
f |-*-** x = f |*>>>>> (-*-**) x
(|*--**) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f2 (f3 a) -> f1 (f2 (f3 (f4 (f5 b))))
f |*--** x = f |*>>>>> (*--**) x
(|*-*-*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f2 (f4 a) -> f1 (f2 (f3 (f4 (f5 b))))
f |*-*-* x = f |*>>>>> (*-*-*) x
(|-**-*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f4 a) -> f1 (f2 (f3 (f4 (f5 b))))
f |-**-* x = f |*>>>>> (-**-*) x
(|**--*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f3 (f4 a) -> f1 (f2 (f3 (f4 (f5 b))))
f |**--* x = f |*>>>>> (**--*) x
(|**-*-) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f3 (f5 a) -> f1 (f2 (f3 (f4 (f5 b))))
f |**-*- x = f |*>>>>> (**-*) x
(|*-**-) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f2 (f5 a) -> f1 (f2 (f3 (f4 (f5 b))))
f |*-**- x = f |*>>>>> (*-**) x
(|-***-) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f5 a) -> f1 (f2 (f3 (f4 (f5 b))))
f |-***- x = f |*>>>>> (-***) x
(|***--) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f4 (f5 a) -> f1 (f2 (f3 (f4 (f5 b))))
f |***-- x = f |*>>>>> (.***) x
infixl 3  |---**, |--*-*, |-*--*, |*---*, |*--*-, |-*-*-, |--**-, |-**--, |*-*--, |**---
(|---**) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f3 a)) -> f1 (f2 (f3 (f4 (f5 b))))
f |---** x = f |*>>>>> (---**) x
(|--*-*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f4 a)) -> f1 (f2 (f3 (f4 (f5 b))))
f |--*-* x = f |*>>>>> (--*-*) x
(|-*--*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f3 (f4 a)) -> f1 (f2 (f3 (f4 (f5 b))))
f |-*--* x = f |*>>>>> (-*--*) x
(|*---*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f2 (f3 (f4 a)) -> f1 (f2 (f3 (f4 (f5 b))))
f |*---* x = f |*>>>>> (*---*) x
(|*--*-) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f2 (f3 (f5 a)) -> f1 (f2 (f3 (f4 (f5 b))))
f |*--*- x = f |*>>>>> (*--*) x
(|-*-*-) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f3 (f5 a)) -> f1 (f2 (f3 (f4 (f5 b))))
f |-*-*- x = f |*>>>>> (-*-*) x
(|--**-) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f5 a)) -> f1 (f2 (f3 (f4 (f5 b))))
f |--**- x = f |*>>>>> (--**) x
(|-**--) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f4 (f5 a)) -> f1 (f2 (f3 (f4 (f5 b))))
f |-**-- x = f |*>>>>> (-**) x
(|*-*--) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f2 (f4 (f5 a)) -> f1 (f2 (f3 (f4 (f5 b))))
f |*-*-- x = f |*>>>>> (*-*) x
(|**---) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f3 (f4 (f5 a)) -> f1 (f2 (f3 (f4 (f5 b))))
f |**--- x = f |*>>>>> (.**) x
infixl 3  |----*, |---*-, |--*--, |-*---, |*----
(|----*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f3 (f4 a))) -> f1 (f2 (f3 (f4 (f5 b))))
f |----* x = f |*>>>>> (----*) x
(|---*-) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f3 (f5 a))) -> f1 (f2 (f3 (f4 (f5 b))))
f |---*- x = f |*>>>>> (---*) x
(|--*--) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f4 (f5 a))) -> f1 (f2 (f3 (f4 (f5 b))))
f |--*-- x = f |*>>>>> (--*) x
(|-*---) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f3 (f4 (f5 a))) -> f1 (f2 (f3 (f4 (f5 b))))
f |-*--- x = f |*>>>>> (-*) x
(|*----) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f2 (f3 (f4 (f5 a))) -> f1 (f2 (f3 (f4 (f5 b))))
f |*---- x = f |*>>>>> (.*) x

infixl 3  -****|, *-***|, **-**|, ***-*|, ****-|
(-****|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 a -> f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f3 (f4 (f5 b))))
x -****| f = (-****) x <<<<<*| f
(*-***|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f2 a -> f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f3 (f4 (f5 b))))
x *-***| f = (*-***) x <<<<<*| f
(**-**|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f3 a -> f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f3 (f4 (f5 b))))
x **-**| f = (**-**) x <<<<<*| f
(***-*|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f4 a -> f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f3 (f4 (f5 b))))
x ***-*| f = (***-*) x <<<<<*| f
(****-|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f5 a -> f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f3 (f4 (f5 b))))
x ****-| f = (.****) x <<<<<*| f
infixl 3  --***|, -*-**|, *--**|, *-*-*|, -**-*|, **--*|, **-*-|, *-**-|, -***-|, ***--| 
(--***|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 a) -> f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f3 (f4 (f5 b))))
x --***| f = (--***) x <<<<<*| f
(-*-**|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f3 a) -> f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f3 (f4 (f5 b))))
x -*-**| f = (-*-**) x <<<<<*| f
(*--**|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f2 (f3 a) -> f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f3 (f4 (f5 b))))
x *--**| f = (*--**) x <<<<<*| f
(*-*-*|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f2 (f4 a) -> f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f3 (f4 (f5 b))))
x *-*-*| f = (*-*-*) x <<<<<*| f
(-**-*|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f4 a) -> f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f3 (f4 (f5 b))))
x -**-*| f = (-**-*) x <<<<<*| f
(**--*|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f3 (f4 a) -> f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f3 (f4 (f5 b))))
x **--*| f = (**--*) x <<<<<*| f
(**-*-|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f3 (f5 a) -> f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f3 (f4 (f5 b))))
x **-*-| f = (**-*) x <<<<<*| f
(*-**-|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f2 (f5 a) -> f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f3 (f4 (f5 b))))
x *-**-| f = (*-**) x <<<<<*| f
(-***-|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f5 a) -> f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f3 (f4 (f5 b))))
x -***-| f = (-***) x <<<<<*| f
(***--|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f4 (f5 a) -> f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f3 (f4 (f5 b))))
x ***--| f = (.***) x <<<<<*| f
infixl 3  ---**|, --*-*|, -*--*|, *---*|, *--*-|, -*-*-|, --**-|, -**--|, *-*--|, **---|
(---**|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 a)) -> f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f3 (f4 (f5 b))))
x ---**| f = (---**) x <<<<<*| f
(--*-*|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f4 a)) -> f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f3 (f4 (f5 b))))
x --*-*| f = (--*-*) x <<<<<*| f
(-*--*|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f3 (f4 a)) -> f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f3 (f4 (f5 b))))
x -*--*| f = (-*--*) x <<<<<*| f
(*---*|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f2 (f3 (f4 a)) -> f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f3 (f4 (f5 b))))
x *---*| f = (*---*) x <<<<<*| f
(*--*-|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f2 (f3 (f5 a)) -> f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f3 (f4 (f5 b))))
x *--*-| f = (*--*) x <<<<<*| f
(-*-*-|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f3 (f5 a)) -> f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f3 (f4 (f5 b))))
x -*-*-| f = (-*-*) x <<<<<*| f
(--**-|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f5 a)) -> f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f3 (f4 (f5 b))))
x --**-| f = (--**) x <<<<<*| f
(-**--|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f4 (f5 a)) -> f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f3 (f4 (f5 b))))
x -**--| f = (-**) x <<<<<*| f
(*-*--|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f2 (f4 (f5 a)) -> f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f3 (f4 (f5 b))))
x *-*--| f = (*-*) x <<<<<*| f
(**---|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f3 (f4 (f5 a)) -> f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f3 (f4 (f5 b))))
x **---| f = (.**) x <<<<<*| f
infixl 3  ----*|, ---*-|, --*--|, -*---|, *----|
(----*|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 a))) -> f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f3 (f4 (f5 b))))
x ----*| f = (----*) x <<<<<*| f
(---*-|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f5 a))) -> f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f3 (f4 (f5 b))))
x ---*-| f = (---*) x <<<<<*| f
(--*--|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f4 (f5 a))) -> f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f3 (f4 (f5 b))))
x --*--| f = (--*) x <<<<<*| f
(-*---|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f3 (f4 (f5 a))) -> f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f3 (f4 (f5 b))))
x -*---| f = (-*) x <<<<<*| f
(*----|) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f2 (f3 (f4 (f5 a))) -> f1 (f2 (f3 (f4 (f5 (a -> b))))) -> f1 (f2 (f3 (f4 (f5 b))))
x *----| f = (.*) x <<<<<*| f

infixl 5  <<<<<*, *>>>>>
(*>>>>>) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 a)))) -> f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f3 (f4 (f5 b))))
f *>>>>> x = f <$|(*>>>>)|*> x
(<<<<<*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 a)))) -> f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f3 (f4 (f5 a))))
x <<<<<* f = x <$|(<<<<*)|*> f

infixl 5  <----*, <---*-, <--*--, <-*---, <*----
(<----*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f3 (f4 a))) -> f1 (f2 (f3 (f4 (f5 b))))
x <----* a = x <<<<<* (----*) a
(<---*-) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f3 (f5 a))) -> f1 (f2 (f3 (f4 (f5 b))))
x <---*- a = x <<<<<* (---*) a
(<--*--) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f4 (f5 a))) -> f1 (f2 (f3 (f4 (f5 b))))
x <--*-- a = x <<<<<* (--*) a
(<-*---) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f3 (f4 (f5 a))) -> f1 (f2 (f3 (f4 (f5 b))))
x <-*--- a = x <<<<<* (-*) a
(<*----) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 b)))) -> f2 (f3 (f4 (f5 a))) -> f1 (f2 (f3 (f4 (f5 b))))
x <*---- a = x <<<<<* (.*) a
infixl 5  <---**, <--*-*, <-*--*, <*---*, <*--*-, <-*-*-, <--**-, <-**--, <*-*--, <**---
(<---**) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f3 a)) -> f1 (f2 (f3 (f4 (f5 b))))
x <---** a = x <<<<<* (---**) a
(<--*-*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f4 a)) -> f1 (f2 (f3 (f4 (f5 b))))
x <--*-* a = x <<<<<* (--*-*) a
(<-*--*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f3 (f4 a)) -> f1 (f2 (f3 (f4 (f5 b))))
x <-*--* a = x <<<<<* (-*--*) a
(<*---*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 b)))) -> f2 (f3 (f4 a)) -> f1 (f2 (f3 (f4 (f5 b))))
x <*---* a = x <<<<<* (*---*) a
(<*--*-) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 b)))) -> f2 (f3 (f5 a)) -> f1 (f2 (f3 (f4 (f5 b))))
x <*--*- a = x <<<<<* (*--*) a
(<-*-*-) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f3 (f5 a)) -> f1 (f2 (f3 (f4 (f5 b))))
x <-*-*- a = x <<<<<* (-*-*) a
(<--**-) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f5 a)) -> f1 (f2 (f3 (f4 (f5 b))))
x <--**- a = x <<<<<* (--**) a
(<-**--) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f4 (f5 a)) -> f1 (f2 (f3 (f4 (f5 b))))
x <-**-- a = x <<<<<* (-**) a
(<*-*--) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 b)))) -> f2 (f4 (f5 a)) -> f1 (f2 (f3 (f4 (f5 b))))
x <*-*-- a = x <<<<<* (*-*) a
(<**---) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 b)))) -> f3 (f4 (f5 a)) -> f1 (f2 (f3 (f4 (f5 b))))
x <**--- a = x <<<<<* (.**) a
infixl 5  <--***, <-*-**, <*--**, <*-*-*, <-**-*, <**--*, <**-*-, <*-**-, <-***-, <***--
(<--***) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 a) -> f1 (f2 (f3 (f4 (f5 b))))
x <--*** a = x <<<<<* (--***) a
(<-*-**) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f3 a) -> f1 (f2 (f3 (f4 (f5 b))))
x <-*-** a = x <<<<<* (-*-**) a
(<*--**) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 b)))) -> f2 (f3 a) -> f1 (f2 (f3 (f4 (f5 b))))
x <*--** a = x <<<<<* (*--**) a
(<*-*-*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 b)))) -> f2 (f4 a) -> f1 (f2 (f3 (f4 (f5 b))))
x <*-*-* a = x <<<<<* (*-*-*) a
(<-**-*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f4 a) -> f1 (f2 (f3 (f4 (f5 b))))
x <-**-* a = x <<<<<* (-**-*) a
(<**--*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 b)))) -> f3 (f4 a) -> f1 (f2 (f3 (f4 (f5 b))))
x <**--* a = x <<<<<* (**--*) a
(<**-*-) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 b)))) -> f3 (f5 a) -> f1 (f2 (f3 (f4 (f5 b))))
x <**-*- a = x <<<<<* (**-*) a
(<*-**-) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 b)))) -> f2 (f5 a) -> f1 (f2 (f3 (f4 (f5 b))))
x <*-**- a = x <<<<<* (*-**) a
(<-***-) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f5 a) -> f1 (f2 (f3 (f4 (f5 b))))
x <-***- a = x <<<<<* (-***) a
(<***--) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 b)))) -> f4 (f5 a) -> f1 (f2 (f3 (f4 (f5 b))))
x <***-- a = x <<<<<* (.***) a
infixl 5  <-****, <*-***, <**-**, <***-*, <****-
(<-****) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 b)))) -> f1 a -> f1 (f2 (f3 (f4 (f5 b))))
x <-**** a = x <<<<<* (-****) a
(<*-***) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 b)))) -> f2 a -> f1 (f2 (f3 (f4 (f5 b))))
x <*-*** a = x <<<<<* (*-***) a
(<**-**) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 b)))) -> f3 a -> f1 (f2 (f3 (f4 (f5 b))))
x <**-** a = x <<<<<* (**-**) a
(<***-*) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 b)))) -> f4 a -> f1 (f2 (f3 (f4 (f5 b))))
x <***-* a = x <<<<<* (***-*) a
(<****-) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 (f5 b)))) -> f5 a -> f1 (f2 (f3 (f4 (f5 b))))
x <****- a = x <<<<<* (.****) a

infixl 5  ----*>, ---*->, --*-->, -*--->, *---->
(----*>) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f4 a))) -> f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f3 (f4 (f5 b))))
a ----*> x = (----*) a *>>>>> x
(---*->) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 (f5 a))) -> f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f3 (f4 (f5 b))))
a ---*-> x = (---*) a *>>>>> x
(--*-->) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f4 (f5 a))) -> f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f3 (f4 (f5 b))))
a --*--> x = (--*) a *>>>>> x
(-*--->) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f3 (f4 (f5 a))) -> f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f3 (f4 (f5 b))))
a -*---> x = (-*) a *>>>>> x
(*---->) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f2 (f3 (f4 (f5 a))) -> f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f3 (f4 (f5 b))))
a *----> x = (.*) a *>>>>> x
infixl 5  ---**>, --*-*>, -*--*>, *---*>, *--*->, -*-*->, --**->, -**-->, *-*-->, **--->
(---**>) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f3 a)) -> f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f3 (f4 (f5 b))))
a ---**> x = (---**) a *>>>>> x
(--*-*>) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f4 a)) -> f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f3 (f4 (f5 b))))
a --*-*> x = (--*-*) a *>>>>> x
(-*--*>) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f3 (f4 a)) -> f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f3 (f4 (f5 b))))
a -*--*> x = (-*--*) a *>>>>> x
(*---*>) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f2 (f3 (f4 a)) -> f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f3 (f4 (f5 b))))
a *---*> x = (*---*) a *>>>>> x
(*--*->) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f2 (f3 (f5 a)) -> f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f3 (f4 (f5 b))))
a *--*-> x = (*--*) a *>>>>> x
(-*-*->) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f3 (f5 a)) -> f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f3 (f4 (f5 b))))
a -*-*-> x = (-*-*) a *>>>>> x
(--**->) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 (f5 a)) -> f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f3 (f4 (f5 b))))
a --**-> x = (--**) a *>>>>> x
(-**-->) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f4 (f5 a)) -> f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f3 (f4 (f5 b))))
a -**--> x = (-**) a *>>>>> x
(*-*-->) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f2 (f4 (f5 a)) -> f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f3 (f4 (f5 b))))
a *-*--> x = (*-*) a *>>>>> x
(**--->) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f3 (f4 (f5 a)) -> f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f3 (f4 (f5 b))))
a **---> x = (.**) a *>>>>> x
infixl 5  --***>, -*-**>, *--**>, *-*-*>, -**-*>, **--*>, **-*->, *-**->, -***->, ***-->
(--***>) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f2 a) -> f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f3 (f4 (f5 b))))
a --***> x = (--***) a *>>>>> x
(-*-**>) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f3 a) -> f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f3 (f4 (f5 b))))
a -*-**> x = (-*-**) a *>>>>> x
(*--**>) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f2 (f3 a) -> f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f3 (f4 (f5 b))))
a *--**> x = (*--**) a *>>>>> x
(*-*-*>) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f2 (f4 a) -> f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f3 (f4 (f5 b))))
a *-*-*> x = (*-*-*) a *>>>>> x
(-**-*>) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f4 a) -> f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f3 (f4 (f5 b))))
a -**-*> x = (-**-*) a *>>>>> x
(**--*>) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f3 (f4 a) -> f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f3 (f4 (f5 b))))
a **--*> x = (**--*) a *>>>>> x
(**-*->) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f3 (f5 a) -> f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f3 (f4 (f5 b))))
a **-*-> x = (**-*) a *>>>>> x
(*-**->) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f2 (f5 a) -> f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f3 (f4 (f5 b))))
a *-**-> x = (*-**) a *>>>>> x
(-***->) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 (f5 a) -> f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f3 (f4 (f5 b))))
a -***-> x = (-***) a *>>>>> x
(***-->) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f4 (f5 a) -> f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f3 (f4 (f5 b))))
a ***--> x = (.***) a *>>>>> x
infixl 5  -****>, *-***>, **-**>, ***-*>, ****->
(-****>) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f1 a -> f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f3 (f4 (f5 b))))
a -****> x = (-****) a *>>>>> x
(*-***>) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f2 a -> f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f3 (f4 (f5 b))))
a *-***> x = (*-***) a *>>>>> x
(**-**>) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f3 a -> f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f3 (f4 (f5 b))))
a **-**> x = (**-**) a *>>>>> x
(***-*>) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f4 a -> f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f3 (f4 (f5 b))))
a ***-*> x = (***-*) a *>>>>> x
(****->) :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4, Applicative f5) => f5 a -> f1 (f2 (f3 (f4 (f5 b)))) -> f1 (f2 (f3 (f4 (f5 b))))
a ****-> x = (.****) a *>>>>> x

