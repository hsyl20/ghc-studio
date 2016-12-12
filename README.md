GHC Studio
==========

GHC Studio is a frontend for GHC (Glasgow Haskell Compiler).


Getting started
---------------

Currently we need a patched GHC version:
> git clone -b fix-dumps https://github.com/hsyl20/ghc.git
> cd ghc
> ./boot
> ./configure
> make -j8


> git clone https://github.com/hsyl20/ghc-studio.git
> cd ghc-studio
> cabal sandbox init

We need to create to make sure that cabal uses the patched GHC:
> cabal configure -w /path/to/ghc/inplace/bin/ghc-stage2
> cabal install --only-dependencies
> cabal build
> ./dist/build/ghc-studio/ghc-studio Main.hs A.hs Test/B.hs -p 9000
Starting Web server at localhost:9000
