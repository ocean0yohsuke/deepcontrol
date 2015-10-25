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
Add `deepcontrol-0.4.3.0` to your extra-deps field in stack.yaml too:

stack.yaml:

    extra-deps:
    ...
    - deepcontrol-0.4.3.0

And type as below:

    .../yourproject$ stack build

Stack must fetch and install `deepcontrol` automatically.

    ../yourproject$ stack build
    deepcontrol-0.4.3.0: configure
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

