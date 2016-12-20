(cd ~/repo/ghc/ghc-studio/inplace/bin; ln -s ghc-stage2 ghc)
export PATH=/home/hsyl20/repo/ghc/ghc-studio/inplace/bin:$PATH
cabal sandbox init
cabal configure --allow-newer=base
cabal install --only-dependencies --allow-newer=base
cabal build

