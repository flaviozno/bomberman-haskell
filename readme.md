## Install System.Random ##
Utilizamos o __System.Random__ para gerar os numeros de forma aleatório para  o tabuleiro.

Para instalar ele utilizamos:
```script
https://www.haskell.org/cabal/download.html --> windows

sudo apt install cabal-install --> linux

cabal install --lib --package-env . random
cabal update
ghci
import System.Random
:l arquivo.hs
```