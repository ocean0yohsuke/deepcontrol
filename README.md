# deepcontrol

A Haskell library that enables more deeper level style programming than the usual Control.xxx modules provide, especially for Applicative and Monad.

## Examples

### [Applicative](https://hackage.haskell.org/package/deepcontrol-0.4.3.0/docs/DeepControl-Applicative.html)

This module enables you to program in applicative style for more deeper level than the usual Applicative module expresses.
You would soon realize exactly what more deeper level means by reading the example codes below in order.

    Prelude> :m DeepControl.Applicative

#### Level-0

bra-ket notation:

    > (1+) |> 2
    3
    > 1 <| (+2)
    3

    > 1 <|(+)|> 2
    3
    > 1 <|(+)|> 2 <|(*)|> 3
    9

    > 1 <|(,)|> 2
    (1,2)

#### Level-1

bra-ket notation:

    > (1+) |$> [2]
    [3]
    > [1] <$| (+2)
    [3]
    > ("<"++)|$> ["a","b"] <$|(++">")
    ["<a>","<b>"]

    > [(1+)] |*> [2]
    [3]

    > [1] <$|(+)|*> [2]
    [3]
    > [1] <$|(+)|*> [0,1,2]
    [1,2,3]
    > [0,1] <$|(+)|*> [2,3] <$|(+)|*> [4,5]
    [6,7,7,8,7,8,8,9]

    > foldr (\x acc -> x <$|(:)|*> acc) ((*:) []) [Just 1, Just 2,  Just 3]
    Just [1,2,3]
    > foldr (\x acc -> x <$|(:)|*> acc) ((*:) []) [Just 1, Nothing, Just 3]
    Nothing

    > filter (even <$|(&&)|*> (10 >)) [1..100]
    [2,4,6,8]
    > filter (even <$|(&&)|*> (10 >) <$|(&&)|*> (5 <)) [1..100]
    [6,8]

cover notation:

    > :t (*:)
    (*:) :: Applicative f => a -> f a

    > (*:) 1 :: Maybe Int
    Just 1
    > (*:) 1 :: [Int]
    [1]
    > (*:) 1 :: Either () Int
    Right 1

cover-braket notation:

    > :t (|*)
    (|*) :: Applicative f => f (a -> b) -> a -> f b

    > [(1+)] |* 2
    [3]
    > [1] <$|(+)|* 2
    [3]

    > (,) |$> ["a1","a2"] |* 'b'
    [("a1",'b'),("a2",'b')]

    > (,,) 'a' |$> ["b1","b2"] |* 'c'
    [('a',"b1",'c'),('a',"b2",'c')]

    > (,,,) 'a' |$> ["b1","b2"] |* 'c' |* 'd'
    [('a',"b1",'c','d'),('a',"b2",'c','d')]
    > (,,,) 'a' |$> ["b1","b2"] |* 'c' |*> ["d1","d2"]
    [('a',"b1",'c',"d1"),('a',"b1",'c',"d2"),('a',"b2",'c',"d1"),('a',"b2",'c',"d2")]

    > 1 *| [(+2)]
    [3]
    > 1 *| [(+)] |* 2
    [3]
    > 1 *|[(+),(-),(*),(^)]|* 2
    [3,-1,2,1]

    > 1 *|Just (,)|* 2
    Just (1,2)

#### Level-2

bra-ket notation:

    > (+1) |$>> [[2]]
    [[3]]
    > [[2]] <<$| (+1)
    [[3]]

    > [Just 1] <<$|(+)|*>> [Just 2]
    [Just 3]
    > [Just 1] <<$|(,)|*>> [Just 2]
    [Just (1,2)]

    > [[1]] <<$|(+)|*>> [[2]] <<$|(-)|*>> [[3]]
    [[0]]

    > foldr (\n acc -> n <<$|(+)|*>> acc) ((**:) 0) [Right (Just 1), Right (Just 2), Right (Just 3)] :: Either () (Maybe Int)
    Right (Just 6)
    > foldr (\n acc -> n <<$|(+)|*>> acc) ((**:) 0) [Right (Just 1), Right Nothing, Right (Just 3)] :: Either () (Maybe Int)
    Right Nothing
    > foldr (\n acc -> n <<$|(+)|*>> acc) ((**:) 0) [Right (Just 1), Right Nothing, Left ()]
    Left ()

cover notation:

    > :t (**:)
    (**:) :: (Applicative f1, Applicative f2) => a -> f1 (f2 a)
    > :t (-*)
    (-*) :: (Applicative f1, Applicative f2) => f1 a -> f1 (f2 a)

    > (**:) 1 :: Maybe [Int]
    Just [1]
    > (-*) (Just 1) :: Maybe [Int]
    Just [1]
    > (*:) [1] :: Maybe [Int]
    Just [1]

cover-braket notation:

    > :t (|**)
    (|**) :: (Applicative f1, Applicative f2) => f1 (f2 (a -> b)) -> a -> f1 (f2 b)

    > [Just 1] <<$|(+)|** 2
    [Just 3]
    > 1 **|(+)|$>> [Just 2]
    [Just 3]
    > 1 **|[Just (+)]|**  2
    [Just 3]
    > 1 **|[Just (+), Just (-), Just (*), Nothing]|** 2
    [Just 3,Just (-1),Just 2,Nothing]

    > :t (|-*)
    (|-*) :: (Applicative f1, Applicative f2) => f1 (f2 (a -> b)) -> f1 a -> f1 (f2 b)
    > :t (|*-)
    (|*-) :: (Applicative f1, Applicative f2) => f1 (f2 (a -> b)) -> f2 a -> f1 (f2 b)

    > [Just 1] <<$|(+)|-* [2]
    [Just 3]
    > [Just 1] <<$|(+)|*- Just 2
    [Just 3]
    >      [1]  -*|(+)|$>> [Just 2]
    [Just 3]
    >   Just 1  *-|(+)|$>> [Just 2]
    [Just 3]
    >   Just 1  *-|[Just (+)]|** 2
    [Just 3]
    >   Just 1  *-|[Just (+)]|*- Just 2
    [Just 3]
    >      [1]  -*|[Just (+)]|*- Just 2
    [Just 3]
    >      [1]  -*|[Just (+), Just (-), Just (*), Nothing]|*- Just 2
    [Just 3,Just (-1),Just 2,Nothing]
    >    [1,2]  -*|[Just (+), Just (-), Just (*), Nothing]|*- Just 2
    [Just 3,Just (-1),Just 2,Nothing,Just 4,Just 0,Just 4,Nothing]

#### Level-3, Level-4 and Level-5

Work well likewise.

### [Commutative](https://hackage.haskell.org/package/deepcontrol-0.4.3.0/docs/DeepControl-Commutative.html)

    Prelude> :m DeepControl.Commutative

[], Maybe, Either, Except and Writer monads are all commutative each other.

    > :t commute
    commute :: (Applicative f, Commutative c) => c (f a) -> f (c a)

    > commute $ Just [1]
    [Just 1]
    > commute $ [Just 1]
    Just [1]

    > commute $ Right (Just 1)
    Just (Right 1)
    > commute $ Just (Right 1)
    Right (Just 1)

So these monads can be deepened freely.

### [Monad](https://hackage.haskell.org/package/deepcontrol-0.4.3.0/docs/DeepControl-Monad.html)

This module enables you to program in Monad for more deeper level than the usual Monad module expresses.
You would soon realize exactly what more deeper level means by reading the example codes below in order.

#### Level-0

```haskell
import DeepControl.Monad ((>-))

plus :: Int -> Int -> Int
plus x y = 
    x >- \a ->   -- (>-) is the level-0 bind function, analogous for (>>=)
    y >- \b ->
    a + b

-- > plus 3 4
-- 7
```

#### Level-2

```haskell
import DeepControl.Applicative ((**:))
import DeepControl.Monad ((>>==))

listlist :: [[String]]             -- List-List monad
listlist = [["a","b"]] >>== \x ->  -- (>>==) is the level-2 bind function, analogous for (>>=)
           [[0],[1,2]] >>== \y ->
           (**:) $ x ++ show y

-- > listlist
-- [["a0","b0"],["a0","b1","b2"],["a1","a2","b0"],["a1","a2","b1","b2"]]
```

```haskell
import DeepControl.Applicative ((|$>), (-*), (*:), (**:))
import DeepControl.Monad ((>>), (>>==), (->~))
import Control.Monad.Writer

factorial :: Int ->
             Maybe (Writer [Int] Int)               -- Maybe-Writer monad
factorial n | n < 0  = (-*) Nothing
            | n == 0 = (*:) $ tell [0] >> (*:) 1
            | n > 0  = factorial (n-1) >>== \v ->   
                       tell [v] ->~                 -- (->~) is a level-2 cover-sequence function, analogous for (>>)
                       (**:) (n * v)

-- > runWriter |$> factorial 5
-- Just (120,[0,1,1,2,6,24])
```

#### Level-3

```haskell
import DeepControl.Applicative ((|$>>), (*-*), (*:), (**:), (***:))
import DeepControl.Monad ((>>), (>>>==), (>--~), (-->~))
import Control.Monad.Writer

factorial :: Int ->
             IO (Maybe (Writer [Int] Int))            -- IO-Maybe-Writer monad
factorial n | n < 0  = (*-*) Nothing                  -- (*-*) is a level-3 cover function
            | n == 0 = (**:) $ tell [0] >> (*:) 1
            | n > 0  = factorial (n-1) >>>== \v ->    -- (>>>==) is the level-3 bind function, analogous for (>>=)
                       print v >--~                   -- (>--~) is a level-3 cover-sequence function, analogous for (>>)
                       tell [v] -->~                  -- (-->~) is a level-3 cover-sequence function too, analogous for (>>)
                       (***:) (n * v)

-- > runWriter |$>> factorial 5
-- 1
-- 1
-- 2
-- 6
-- 24
-- Just (120,[0,1,1,2,6,24])
```
#### Level-4 and Level-5

Work well likewise.

### [Monad-Transformer](https://hackage.haskell.org/package/deepcontrol-0.4.3.0/docs/DeepControl-Monad-Trans.html)

#### Level-2

Here is a monad transformer example how to implement Ackermann function, improved to stop within a certain limit of time, with ReaderT-IdentityT2-IO-Maybe monad, a level-2 monad-transformation.

```haskell
import DeepControl.Applicative
import DeepControl.Commutative (commute)
import DeepControl.Monad ((>-))
import DeepControl.Monad.Morph ((|>|))
import DeepControl.Monad.Trans (liftTT2, transfold2, untransfold2)
import DeepControl.Monad.Trans.Identity
import Control.Monad.Reader
import Control.Monad.Trans.Maybe

import System.Timeout (timeout)

type TimeLimit = Int

ackermannTimeLimit :: TimeLimit -> Int -> Int -> 
                      IO (Maybe Int)                     -- IO-Maybe monad
ackermannTimeLimit timelimit x y = timeout timelimit (ackermannIO x y)
  where
    ackermannIO :: Int -> Int -> IO Int
    ackermannIO 0 n = (*:) $ n + 1
    ackermannIO m n | m > 0 && n == 0 = ackermannIO (m-1) 1
                    | m > 0 && n > 0  = ackermannIO m (n-1) >>= ackermannIO (m-1)
 
ackermann :: Int -> Int -> 
             ReaderT TimeLimit (IdentityT2 IO Maybe) Int -- ReaderT-IdentityT2-IO-Maybe monad
ackermann x y = do
    timelimit <- ask
    liftTT2 $ ackermannTimeLimit timelimit x y           -- lift IO-Maybe function to ReaderT-IdentityT2-IO-Maybe function

calc_ackermann :: TimeLimit -> Int -> Int -> IO (Maybe Int)
calc_ackermann timelimit x y = ackermann x y >- \r -> runReaderT r timelimit
                                             >- runIdentityT2

-- λ> commute $ calc_ackermann 1000 |$> [0..4] |* 4
-- [Just 5,Just 6,Just 11,Just 125,Nothing]

ackermann' :: Int -> Int -> 
              ReaderT TimeLimit (MaybeT IO) Int                -- ReaderT-MaybeT-IO monad
ackermann' x y = (runIdentityT . transfold2) |>| ackermann x y -- You can get usual ReaderT-MaybeT-IO function from ReaderT-IdentityT2-IO-Maybe function

ackermann'' :: Int -> Int -> 
               ReaderT TimeLimit (IdentityT2 IO Maybe) Int      -- ReaderT-IdentityT2-IO-Maybe monad
ackermann'' x y = (untransfold2 . IdentityT) |>| ackermann' x y -- You can get ReaderT-IdentityT2-IO-Maybe function from usual ReaderT-MaybeT-IO function
```

Here is a monad transformer example showing how to use trans-cover functions.

```haskell
import DeepControl.Applicative ((|$>))
import DeepControl.Commutative (Commutative)
import DeepControl.Monad (Monad)
import DeepControl.Monad.Morph (generalize, (|>|))
import DeepControl.Monad.Trans (liftT)
import DeepControl.Monad.Trans.Identity (IdentityT2(..), (|-*|), (|*-|))
import Control.Monad.Writer
import Control.Monad.State

tick :: State Int ()
tick = modify (+1)

tock                         ::                   StateT Int IO ()
tock = do
    generalize |>| tick      :: (Monad      m) => StateT Int m  ()  -- (|>|) is the level-1 trans-map function, analogous for (|$>)
    liftT $ putStrLn "Tock!" :: (MonadTrans t) => t          IO ()  -- 'liftT' is the level-1 trans-lift function, alias to 'lift'

-- λ> runStateT tock 0
-- Tock!
-- ((),1)

save :: StateT Int (Writer [Int]) ()
save = do
    n <- get
    liftT $ tell [n]

program ::                             StateT Int (IdentityT2 IO (Writer [Int])) () -- StateT-IdentityT2-IO-Writer monad, a level-2 monad-transform
program = replicateM_ 4 $ do
    ((|-*|).liftT) |>| tock                                                         -- (|-*|) is a level-2 trans-cover function, analogous for (-*)
        :: (Monad m, Commutative m) => StateT Int (IdentityT2 IO m             ) ()
    ((|*-|).liftT) |>| save                                                         -- (|*-|) is a level-2 trans-cover function, analogous for (*:)
        :: (Monad m               ) => StateT Int (IdentityT2 m  (Writer [Int])) ()

-- λ> execWriter |$> runIdentityT2 (runStateT program 0)
-- Tock!
-- Tock!
-- Tock!
-- Tock!
-- [1,2,3,4]
```
#### Level-3, Level-4 and Level-5

Work well likewise.

### [Monad-Morph](https://hackage.haskell.org/package/deepcontrol-0.4.3.0/docs/DeepControl-Monad-Morph.html)

Here is a monad-morph example, a level-2 monad-morph.

```haskell
import DeepControl.Monad.Morph
import Control.Monad.Writer
import Control.Monad.State

-- i.e. :: StateT Int Identity ()
tick    :: State Int ()
tick = modify (+1)

tock                        ::                   StateT Int IO ()
tock = do
    generalize |>| tick     :: (Monad      m) => StateT Int m  ()  -- (|>|) is the level-1 trans-map function, analogous for (|$>)
    lift $ putStrLn "Tock!" :: (MonadTrans t) => t          IO ()

-- λ> runStateT tock 0
-- Tock!
-- ((),1)

-- i.e. :: StateT Int (WriterT [Int] Identity) ()
save    :: StateT Int (Writer  [Int]) ()
save = do
    n <- get
    lift $ tell [n]

program ::                   StateT Int (WriterT [Int] IO) ()
program = replicateM_ 4 $ do
    lift |>| tock
        :: (MonadTrans t) => StateT Int (t             IO) ()
    generalize |>>| save                                        -- (|>>|) is the level-2 trans-map function, analogous for (|$>>)
        :: (Monad      m) => StateT Int (WriterT [Int] m ) ()

-- λ> execWriterT (runStateT program 0)
-- Tock!
-- Tock!
-- Tock!
-- Tock!
-- [1,2,3,4]
```

#### Level-3, Level-4 and Level-5

Work well likewise.

### [Arrow](https://hackage.haskell.org/package/deepcontrol-0.4.3.0/docs/DeepControl-Arrow.html)
