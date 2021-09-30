{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import System.Random
import System.IO.Unsafe

{-
        Alunos: Flávio Vezono Filho 11921BCC014
                Guilherme Nascimento Leite 11921BCC018 
                Yasmin Marques Vieira 11921BCC025
-}

data Item = Grama | Parede | Pedra | Bomba | Arremesso | Patins | Fogo | Jogador1 | Jogador2 | Jogador3  deriving (Show,Eq,Read)
data Direcao = N | S | L | O deriving (Eq,Show)

type Celula = [Item]
type Linha = [Celula]
type Tabuleiro = [Linha]
type Capacidade = ((Item, Int), (Item, Int), (Item, Int)) 
type Jogador = (Int, (Int, Int), Direcao, Capacidade)

vazio :: Celula -> Bool
vazio pilha
        | null pilha = True
        | otherwise = False

presente :: Item -> Char
presente x
        | x == Arremesso = 'A'
        | x == Patins = 'P'
        | x == Fogo = 'F'


pilha :: Celula -> Bool
pilha [] = True
pilha [x] = x == Grama || x == Parede || x == Pedra
pilha (x:xs)
        |x == Grama || x == Parede || x == Pedra = null xs
        |x == Arremesso || x == Patins || x == Fogo || x == Bomba || x == Jogador1 || x == Jogador2 || x == Jogador3 || x == Parede = a == Grama && pilha xs
        |otherwise = a == Grama || presente a == 'A' || presente a == 'P' -- Ou é grama ou presente
        where a = head xs

validaCelula :: Celula -> Bool
validaCelula [] = True
validaCelula celula = pilha celula

-- rand :: IO Int 
-- rand = randomRIO (1, 10)

rlist :: [Int]
rlist = take 9 $ randomRs (1,10) (mkStdGen 2)

-- CRIA TABULEIRO
geraTabuleiro :: Tabuleiro
geraTabuleiro = [geraLinha 0 0 7 rlist, geraLinha 1 0 7 rlist, geraLinha 2 0 7 rlist, geraLinha 3 0 7 rlist, geraLinha 4 0 7 rlist, geraLinha 5 0 7 rlist, geraLinha 6 0 7 rlist, geraLinha 7 0 7 rlist]

--geraLinha :: Int -> Int -> Int -> Linha -> Linha
-- >>>geraLinha 1 0 7
-- [[Pedra],[Grama],[Grama],[Grama],[Grama],[Grama],[Jogador1,Grama],[Pedra]]
geraLinha :: Int -> Int -> Int -> [Int] -> Linha
geraLinha l c t listaRand
                 | l == 0 && c == 0 = [Pedra] : geraLinha l (c+1) t listaRand
                 | (l == 0 || l == t) && c < t = [Pedra] : geraLinha l (c+1) t listaRand-- 1ª e ultima linha
                 | c == t && l <= t =  [Pedra] : [] -- ultima coluna
                 | c == 0 && l /= t = [Pedra]  : geraLinha l (c+1) t listaRand -- inicio da linha
                 | l == 1 && c == 6 = [Jogador1, Grama] : geraLinha l (c+1) t listaRand -- seta jogador 1
                 | l == 1 && c < (t-1) = [Grama] : geraLinha l (c+1) t listaRand -- linha superior
                 | even l && even c && c /= t  = [Pedra] : geraLinha l (c+1) t listaRand
                 | l == 3 && c == 1 = [Jogador2, Grama] : geraLinha l (c+1) t listaRand-- seta jogador 2
                 | l == 6 && c == 5 = [Jogador3, Grama] : geraLinha l (c+1) t listaRand-- seta jogador 3
                 | otherwise = sortItem (last (take c listaRand)) : geraLinha l (c+1) t listaRand
                

sortItem :: Int -> Celula
sortItem i
            | i == 1 = [Parede]
            | i == 2 = [Grama]
            | i == 3 = [Parede] ++ [Grama]
            | i == 5 = [Patins] ++ [Grama]
            | i == 6 = [Arremesso] ++ [Grama]
            | i == 7 = [Fogo] ++ [Grama]
            | i == 8 = [Parede] ++ [Patins] ++ [Grama]
            | i == 9 = [Parede] ++ [Arremesso] ++ [Grama]
            | i == 10 = [Parede] ++ [Fogo] ++ [Grama]

-- >>> acessaCelula [[[Pedra],[Pedra],[Pedra],[Pedra],[Pedra],[Pedra],[Pedra],[Pedra]],[[Pedra],[Grama],[Grama],[Grama],[Grama],[Grama],[Jogador1,Grama],[Pedra]],[[Pedra],[Fogo,Grama],[Pedra],[Fogo,Grama],[Pedra],[Fogo,Grama],[Pedra],[Pedra]],[[Pedra],[Jogador2,Grama],[Fogo,Grama],[Fogo,Grama],[Fogo,Grama],[Fogo,Grama],[Fogo,Grama],[Pedra]],[[Pedra],[Fogo,Grama],[Pedra],[Fogo,Grama],[Pedra],[Fogo,Grama],[Pedra],[Pedra]],[[Pedra],[Fogo,Grama],[Fogo,Grama],[Fogo,Grama],[Fogo,Grama],[Fogo,Grama],[Fogo,Grama],[Pedra]],[[Pedra],[Fogo,Grama],[Pedra],[Fogo,Grama],[Pedra],[Jogador3,Grama],[Pedra],[Pedra]],[[Pedra],[Pedra],[Pedra],[Pedra],[Pedra],[Pedra],[Pedra],[Pedra]]] 1 6
-- [Jogador1,Grama]

acessaCelula :: Tabuleiro -> Int -> Int -> Celula
acessaCelula t l c
        | l == 0 && c == 0 = head (head t)  
        | l == 0 && c > 0 =  last (take (c+1) (head t))
        | l > 0 && c == 0 =  head (last (take (l+1) t))
        | otherwise =  last (take (c+1) (last (take (l+1) t)))

jogadorItem :: Int -> Item
jogadorItem id
   | id == 1 = Jogador1
   | id == 2 = Jogador2
   | id == 3 = Jogador3

jogadorId :: Item -> Int
jogadorId item
   | item == Jogador1 = 1
   | item == Jogador2 = 2
   | item == Jogador3 = 3

verificaJogador :: Item -> Bool 
verificaJogador j = j == Jogador1 || j == Jogador2 || j == Jogador3 

-- Cria lista de jogadores
listaJogadores :: Tabuleiro -> Int -> [Jogador]
listaJogadores [] _ = []
listaJogadores (x:xs) l = linha ++ listaJogadores xs (l + 1)
   where
      linha = inicializaJogadores x l 0

-- Inicializa os jogadores
inicializaJogadores :: Linha -> Int -> Int -> [Jogador]
inicializaJogadores [] _ _ = []
inicializaJogadores (x:xs) l c
   | vazio x = inicializaJogadores xs l (c + 1)
   | verificaJogador (head x) == False = inicializaJogadores xs l (c + 1)
   | otherwise = (jogadorId (head x), (l, c), N, ((Patins, 0), (Fogo, 0), (Arremesso, 0))) : inicializaJogadores xs l (c + 1)
  
-- posicaoJogador :: Jogador -> [Jogador] -> Int -> Int
-- posicaoJogador j js = snd (take (jogadorId j) js)

-- direcaoJogador :: Jogador -> [Jogador] -> Direcao
-- direcaoJogador j js = last (take 3 (take (jogadorId j) js))

-- -- MOVIMENTAÇÃO
-- movimentaJogador :: Tabuleiro -> Jogador -> [Jogador] -> Direcao -> Tabuleiro
-- movimentaJogador t j js d
--         | d == direcaoJogador j js && movimentacaoValida t fst (posicaoJogador j js) snd (posicaoJogador j js) = novo Tab


-- movimentacaoValida :: Tabuleiro -> Int -> Int -> Bool
-- movimentacaoValida t l c
--         | head (acessaCelula t l c) == Pedra = False
--         | head (acessaCelula t l c) == Parede = False 
--         | otherwise = True
