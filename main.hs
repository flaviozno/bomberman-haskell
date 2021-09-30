{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import System.Random
import System.IO.Unsafe

data Item = Grama | Parede | Pedra | Bomba | Arremesso | Patins | Fogo | Jogador deriving (Show,Eq,Read)
data Direcao = N | S | L | O deriving (Eq,Show)

type Celula = [Item]
type Linha = [Celula]
type Tabuleiro = [Linha]
type Capacidade = (Item, Item, Item) -> Int
type Jogador = (Int, (Int, Int), Direcao, Capacidade)

vazio :: Celula -> Bool
vazio pilha
        | null pilha = True
        | otherwise = False

presente :: Item -> Char
presente x
        | x == Arremesso = 'A'
        | x == Patins = 'P'

-- listajogadores :: [Int] -> [Jogador]
-- listajogadores [] = []
-- listajogadores (x:xs) 
--         | (x == 0) || (x == 1)  =  x : listajogadores xs
--         | otherwise = listajogadores xs

pilha :: Celula -> Bool
pilha [] = True
pilha [x] = x == Grama || x == Parede || x == Pedra
pilha (x:xs)
        |x == Grama || x == Parede || x == Pedra = null xs
        |x == Arremesso || x == Patins || x == Fogo || x == Bomba || x == Jogador || x == Parede = a == Grama && pilha xs
        |otherwise = a == Grama || presente a == 'A' || presente a == 'P' -- Ou é grama ou presente
        where a = head xs

validaCelula :: Celula -> Bool
validaCelula [] = True
validaCelula celula = pilha celula

rand :: Int
rand =  unsafePerformIO (getStdRandom (randomR (1, 8)))

geraTabuleiro :: Tabuleiro
geraTabuleiro = [geraLinha 0 0 7 , geraLinha 1 0 7 , geraLinha 2 0 7 , geraLinha 3 0 7 , geraLinha 4 0 7 , geraLinha 5 0 7 , geraLinha 6 0 7, geraLinha 7 0 7 ]

--geraLinha :: Int -> Int -> Int -> Linha -> Linha
-- >>>rand
-- >>>geraLinha 7 0 7
-- 8
-- [[Pedra],[Pedra],[Pedra],[Pedra],[Pedra],[Pedra],[Pedra],[Pedra]]
geraLinha :: Int -> Int -> Int -> Linha
geraLinha l c t
                 | l == 0 && c == 0 = [Pedra] : geraLinha l (c+1) t
                 | (l == 0 || l == t) && c < t = [Pedra] : geraLinha l (c+1) t  -- 1ª e ultima linha
                 | c == t && l <= t =  [Pedra] : [] -- ultima coluna
                 | c == 0 && l /= t = [Pedra]  : geraLinha l (c+1) t  -- inicio da linha
                 | l == 1 && c < (t-1)  = [Grama] : geraLinha l (c+1) t  -- linha superior
                 | even l && even c && c /= t  = [Pedra] : geraLinha l (c+1) t
                 | otherwise = sortItem rand : geraLinha l (c+1) t

--sortItem :: Int -> Celula
sortItem :: (Eq a, Num a) => a -> [Item]
sortItem i
            | i==1 = [Parede]
            | i==2 = [Grama]
            | i==3 = [Grama] ++ [Parede]
            | i==5 = [Grama] ++ [Patins]
            | i==6 = [Grama] ++ [Arremesso]
            | i==7 = [Grama] ++ [Fogo]
            | i==8 = [Grama] ++ [Patins] ++ [Parede]
            | i==9 = [Grama] ++ [Arremesso] ++ [Parede]
            | i==10 = [Grama] ++ [Fogo] ++ [Parede]
