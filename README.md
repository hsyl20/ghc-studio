GHC Studio
==========

GHC Studio is a frontend for GHC (Glasgow Haskell Compiler).


Getting started
---------------

Currently we need a patched GHC version:
> git clone -b ghc-studio https://github.com/hsyl20/ghc.git
> cd ghc
> ./boot
> ./configure
> make -j8
> cd inplace/bin
> ln -s ghc-stage2 ghc


> git clone https://github.com/hsyl20/ghc-studio.git
> cd ghc-studio
> cabal sandbox init

We need to create to make sure that cabal uses the patched GHC:
> export PATH=/path/to/ghc/inplace/bin/:$PATH
> cabal configure 
> cabal install --only-dependencies
> cabal build
> ./dist/build/ghc-studio/ghc-studio Main.hs A.hs Test/B.hs -p 9000
Starting Web server at localhost:9000
