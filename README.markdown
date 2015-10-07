# deepcontrol

A Haskell library that enables more deeper level style programming than the usual Control.xxx modules provide, especially for Applicative and Monad.

## Installing with [Stack](https://github.com/commercialhaskell/stack/blob/master/doc/GUIDE.md)

If you haven't installed Stack yet, install [Stack](https://github.com/commercialhaskell/stack#readme). 

If you have never even used Stack, launch the terminal and go to your working directory:

    .../yourworkingdirectory$

To create your own Stack new project folder, type as below:

    ../yourworkingdirectory$ stack new yourproject simple
    Downloading template "simple" to create project "yourproject" in yourproject/ ...
    ...

Go into your project folder:

    ../yourworkingdirectory$ cd yourproject/

To install GHC on your Stack project folder, type as below:

    .../yourproject$ stack setup
    stack will use a locally installed GHC

Now start ghci and see if it works well.

    .../yourproject$ stack ghci
    ...
    Prelude>

### Fetch from [Stackage](http://www.stackage.org/)

Add `deepcontrol` to your .cabal file:

yourproject.cabal:

      ...
      build-depends:       ...
                         , deepcontrol

On your project folder run "stack build" to get Stack to install `deepcontrol` into your project.

    .../yourproject$ stack build

If Stack yields a messeage below, it means that `deepcontrol` failed to be resolved on yourproject's Stack resolver.
Probably you will get this message since `deepcontrol` is just one of miner libraries yet.

    .../yourproject$ stack build
    While constructing the BuildPlan the following exceptions were encountered:
    ...

If you want to try other resolver, type as below:

    .../yourproject$ stack init
    Refusing to overwrite existing stack.yaml, please delete before running stack init or if you are sure use "--force"

Please follow the message direction.

### Fetch from [Hackage](https://hackage.haskell.org/package/deepcontrol)

Ok, I(you) got `deepcontrol` isn't in Stackage. Then let's fetch `deepcontrol` from Hackage.
Add `deepcontrol-0.1.0.0` to your extra-deps field in stack.yaml too:

stack.yaml:

    extra-deps:
    ...
    - deepcontrol-0.1.0.0

And type as below:

    .../yourproject$ stack build

Stack must fetch and install `deepcontrol` automatically.

    ../yourproject$ stack build
    deepcontrol-0.1.0.0: configure
    ...

Now start ghci and see if it works well.

    .../yourproject$ stack ghci
    ...
    Prelude> :m DeepControl.Applicative

## Installing with Cabal

`deepcontrol` is available from
[Hackage](https://hackage.haskell.org/package/deepcontrol).

Launch the terminal and go to your project folder:

    .../yourproject$

If you haven't done setup cabal sandbox on your project folder yet, type as below so that `deepcontrol` will be installed locally on your project folder:

    .../yourproject$ cabal sandbox init
    Writing a default package environment file to
    ...

To install `deepcontrol` on your project folder, type as below:

    .../yourproject$ cabal update
    Downloading the latest package list from hackage.haskell.org
    ...
    .../yourproject$ cabal install deepcontrol
    Resolving dependencies...
    ...
    
Now start ghci and see if it works well.

    .../yourproject$ cabal repl
    ...

    Prelude> :m DeepControl.Applicative

## Examples

### [Applicative](https://hackage.haskell.org/package/deepcontrol-0.1.0.0/docs/DeepControl-Applicative.html)

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

braket-cover notation

    > [(1+)] |* 2 
    [3] 
    > [1] <$|(+)|* 2 
    [3]
    > [1] <$|(+)|* 2 <$|(*)|* 3 
    [9]

    > Just 1 <$|(,)|* 2 
    Just (1,2)

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

    > foldr (\n acc -> n <<$|(+)|*>> acc) ((**:) 0) ([Right (Just 1), Right (Just 2), Right (Just 3)]) :: Either () (Maybe Int)
    Right (Just 6)
    > foldr (\n acc -> n <<$|(+)|*>> acc) ((**:) 0) ([Right (Just 1), Right Nothing, Right (Just 3)]) :: Either () (Maybe Int)
    Right Nothing
    > foldr (\n acc -> n <<$|(+)|*>> acc) ((**:) 0) ([Right (Just 1), Right Nothing, Left ()])
    Left ()

braket-cover notation:

    > [Just 1] <<$|(+)|** 2 
    [Just 3]
    > 1 **|(+)|$>> [Just 2] 
    [Just 3]
    > 1 **|[Just (+)]|**  2 
    [Just 3]
    > 1 **|[Just (+), Just (-), Just (*), Nothing]|** 2 
    [Just 3,Just (-1),Just 2,Nothing]

    > [Just 1] <<$|(+)|-* [2] 
    [Just 3]
    > [Just 1] <<$|(+)|*- Just 2 
    [Just 3]
    > [1]       -*|(+)|$>> [Just 2] 
    [Just 3]
    > Just 1    *-|(+)|$>> [Just 2] 
    [Just 3]
    > Just 1    *-|[Just (+)]|** 2
    [Just 3]
    > Just 1    *-|[Just (+)]|*- Just 2
    [Just 3]
    > [1]       -*|[Just (+)]|*- Just 2
    [Just 3]
    > [1] -*|[Just (+), Just (-), Just (*), Nothing]|*- Just 2
    [Just 3,Just (-1),Just 2,Nothing]
    > [0,1] -*|[Just (+), Just (-), Just (*), Nothing]|*- Just 2
    [Just 2,Just 3,Just (-2),Just (-1),Just 0,Just 2,Nothing,Nothing]

#### Level-3

Work well likewise.

#### Level-4, Level-5

Not completely written up yet.

### [Monad](https://hackage.haskell.org/package/deepcontrol-0.1.0.0/docs/DeepControl-Monad.html)

This module enables you to program in Monad for more deeper level than the usual Monad module expresses.
You would soon realize exactly what more deeper level means by reading the example codes below in order.

#### Level-2

```haskell
import DeepControl.Applicative ((**:))
import DeepControl.Monad

listlist :: [[String]]             -- List-List Monad
listlist = [["a","b"]] >>== \x -> 
           [[0],[1,2]] >>== \y -> 
           (**:) $ x ++ show y

-- > listlist
-- [["a0","b0"],["a0","b1","b2"],["a1","a2","b0"],["a1","a2","b1","b2"]]
```

```haskell
import DeepControl.Applicative
import DeepControl.Monad

isJust (Just _) = True
isJust _        = False

pythagorean_triples :: [Maybe (Int, Int, Int)]  -- List-Maybe Monad
pythagorean_triples = filter isJust $
    [1..10] >-== \x ->
    [1..10] >-== \y ->
    [1..10] >-== \z ->
    guard (x < y && x*x + y*y == z*z) ->~
    (**:) (x,y,z)

-- > pythagorean_triples
-- [Just (3,4,5),Just (6,8,10)]
```

#### Level-3

```haskell
import DeepControl.Applicative
import DeepControl.Monad

isJust (Just _) = True
isJust _        = False

pythagorean_triples :: IO [Maybe (Int, Int, Int)]  -- IO-List-Maybe Monad
pythagorean_triples = filter isJust |$> (
    [1..10] ->-== \x ->
    [1..10] ->-== \y ->
    [1..10] ->-== \z ->
    guard (x < y && x*x + y*y == z*z) -->~
    print (x,y,z) >--~
    (***:) (x,y,z)
  )

-- > pythagorean_triples
-- (3,4,5)
-- (6,8,10)
-- [Just (3,4,5),Just (6,8,10)]
```

### [Arrow](https://hackage.haskell.org/package/deepcontrol-0.1.0.0/docs/DeepControl-Arrow.html)

### [Commutative](https://hackage.haskell.org/package/deepcontrol-0.1.0.0/docs/DeepControl-Commutative.html)


