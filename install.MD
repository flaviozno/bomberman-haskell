## Install System.Random ##

```script
https://www.haskell.org/cabal/download.html --> windows

sudo apt install cabal-install --> linux

cabal install --lib --package-env . random
cabal update
ghci
import System.Random
:l arquivo.hs
```