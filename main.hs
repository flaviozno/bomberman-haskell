{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Text.Read (readMaybe)

{-
        Alunos: Flávio Vezono Filho 11921BCC014
                Guilherme Nascimento Leite 11921BCC018 
                Yasmin Marques Vieira 11921BCC025
-}



import System.Random ( mkStdGen, Random(randomRs) )
import System.IO.Unsafe ()

data Item = Buraco | Grama | Parede | Pedra | Bomba | Arremesso | Patins | Fogo | Jogador1 | Jogador2 | Jogador3  deriving (Show,Eq,Read)
data Direcao = N | S | L | O deriving (Eq,Show)
data Ação = ColocarBomba | Mover Direcao | NO_OP | Sair deriving (Show, Eq)

type Celula = [Item]
type Linha = [Celula]
type Tabuleiro = [Linha]
type Jogador = (Int, (Int, Int), Direcao, ((Item, Int), (Item, Int), (Item, Int)))

verificaId :: [Int] -> IO Int
verificaId js = do
        print ("Escolha o jogador: " ++ show js)
        opcaoStr <- getLine
        return (maybe (-1) id (readMaybe opcaoStr))

main :: IO ()
main = do
        actionLoop t lj
        where
            t = geraTabuleiro
            lj = listaJogadores t 0

menu :: IO Ação
menu = do
        print "O que deseja fazer: "
        print "Mover Norte -> N"
        print "Mover Sul -> S"
        print "Mover Leste -> L"
        print "Mover Oeste -> O"
        print "Colocar Bomba -> B"
        print "Sair -> E"
        opcao <- getLine
        return (case opcao of "N" -> Mover N
                              "S" -> Mover S
                              "L" -> Mover L
                              "O" -> Mover O
                              "B" -> ColocarBomba
                              "E" -> Sair
                              _ -> NO_OP)

{-printarTab :: Tabuleiro -> IO ()
printarTab [] = return ()
printarTab x = do
    print (head x)
    printarTab (tail x)-}

printarTab :: Tabuleiro -> IO ()
printarTab [] = return ()
printarTab t = do
        print (formarLinha t 0 7)
        print (formarLinha t 1 7)
        print (formarLinha t 2 7)
        print (formarLinha t 3 7)
        print (formarLinha t 4 7)
        print (formarLinha t 5 7)
        print (formarLinha t 6 7)
        print (formarLinha t 7 7)

formarLinha :: Tabuleiro -> Int -> Int -> Linha
formarLinha t l 0 = [[head (acessaCelula t l 0)]]
formarLinha t l c 
                | acessaCelula t l c /= [] = formarLinha t l (c-1) ++ [[head (acessaCelula t l c)]]
                | acessaCelula t l c == [] = formarLinha t l (c-1) ++ [acessaCelula t l c]

actionLoop :: Tabuleiro -> [Jogador] -> IO ()
actionLoop t js =
        let ids = [i | (i, (_, _), _, ((_, _), (_, _), (_, _))) <- js] in
        do
                putStr "\n"
                printarTab t
                putStr "\n"
                putStr "Lista Jogadores: "
                print js
                putStr "\n"
                j <- verificaId ids
                opcao <- menu
                if opcao == Sair then return ()
                        else if j `elem` ids
                                then
                                        let (t', js') = case opcao of Mover d -> movimentaJogador t (pegaJogador j js) js d
                                                                      ColocarBomba -> colocarBomba t (pegaJogador j js) js
                                                                      NO_OP -> (t,js)
                                                                      Sair -> (t,js)
                                        in actionLoop t' js'
                                else actionLoop t js

vazio :: Celula -> Bool
vazio pilha
        | null pilha = True
        | otherwise = False


-- Regras da pilha de acordo com a sobreposicao
pilha :: Celula -> Bool
pilha [] = True
pilha [x] = x == Grama || x == Parede || x == Pedra
pilha (x:xs)
        | (x == Grama || x == Pedra) && xs == [] = True
        | (x == Arremesso || x == Patins || x == Fogo || x == Bomba) && a == Grama = True
        | (x == Jogador1 || x == Jogador2 || x == Jogador3) && a == Grama = True
        | x == Parede && (a == Grama || xs == [] || a == Arremesso || a == Patins || a == Fogo) = pilha xs
        | otherwise = False
        where a = head xs

-- Verifica se uma celula é valida por meio das regras pilha
validaCelula :: Celula -> Bool
validaCelula [] = True
validaCelula celula = pilha celula


-- Lista de numeros aleatorios 
rlist :: [Int]
rlist = take 9 $ randomRs (1,10) (mkStdGen 2)

-- CRIA TABULEIRO

-- Gera linha por linha para formar o tabuleiro
geraLinha :: Int -> Int -> Int -> [Int] -> Linha
geraLinha l c t listaRand
                 | l == 0 && c == 0 = [Pedra] : geraLinha l (c+1) t listaRand
                 | (l == 0 || l == t) && c < t = [Pedra] : geraLinha l (c+1) t listaRand -- 1ª e ultima linha
                 | c == t && l <= t =  [Pedra] : [] -- ultima coluna
                 | c == 0 && l /= t = [Pedra]  : geraLinha l (c+1) t listaRand -- inicio da linha
                 | l == 1 && c == 6 = [Jogador1, Grama] : geraLinha l (c+1) t listaRand -- seta jogador 1
                 | l == 1 && c < (t-1) = [Grama] : geraLinha l (c+1) t listaRand -- linha superior
                 | even l && even c && c /= t  = [Pedra] : geraLinha l (c+1) t listaRand -- linhas pares
                 | l == 3 && c == 1 = [Jogador2, Grama] : geraLinha l (c+1) t listaRand -- seta jogador 2
                 | l == 6 && c == 5 = [Jogador3, Grama] : geraLinha l (c+1) t listaRand -- seta jogador 3
                 | otherwise = sortItem (last (take c listaRand)) : geraLinha l (c+1) t listaRand

geraTabuleiro :: Tabuleiro
geraTabuleiro = [geraLinha 0 0 7 rlist, geraLinha 1 0 7 rlist, geraLinha 2 0 7 rlist, geraLinha 3 0 7 rlist, geraLinha 4 0 7 rlist, geraLinha 5 0 7 rlist, geraLinha 6 0 7 rlist, geraLinha 7 0 7 rlist]


-- Match da celula 
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


-- Acessa uma coordenada do mapa e retorna a celula presente
acessaCelula :: Tabuleiro -> Int -> Int -> Celula
acessaCelula t l c
        | l == 0 && c == 0 = head (head t)
        | l == 0 && c > 0 =  last (take (c+1) (head t))
        | l > 0 && c == 0 =  head (last (take (l+1) t))
        | otherwise =  last (take (c+1) (last (take (l+1) t)))

-- Retorna o id do jogador
jogadorId :: Item -> Int
jogadorId item
   | item == Jogador1 = 1
   | item == Jogador2 = 2
   | item == Jogador3 = 3

-- Retorna o respectivo item JogadorX 
idJogador :: Int -> Item
idJogador id
        | id == 1 = Jogador1
        | id == 2 = Jogador2
        | id == 3 = Jogador3

-- Verifica se um Item é um jogador
verificaJogador :: Item -> Bool
verificaJogador j = j == Jogador1 || j == Jogador2 || j == Jogador3

-- Acesso a estrutura de um dado jogador
pegaJogador :: Int -> [Jogador] -> Jogador
pegaJogador j lj = last (take j lj)

-- Cria a lista de jogadores do tabuleiro
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


-- MOVIMENTAÇÃO

-- Verifica se a movimentaçao pode ser executada 
movimentacaoValida :: Tabuleiro -> Int -> Int -> Bool
movimentacaoValida t l c
        | head (acessaCelula t l c) == Pedra = False
        | head (acessaCelula t l c) == Parede = False
        | head (acessaCelula t l c) == Bomba = False
        | head (acessaCelula t l c) == Jogador1 || head (acessaCelula t l c) == Jogador2 || head (acessaCelula t l c) == Jogador3 = False
        | otherwise = True

movimentaJogador :: Tabuleiro -> Jogador -> [Jogador] -> Direcao -> (Tabuleiro, [Jogador])
movimentaJogador t (id, (l, c), dir, ((Patins, p), (Fogo, f), (Arremesso, a))) lj d
        | dir /= d = (t, novaLista)
        | dir == d && d == N && movimentacaoValida t (l-1) c = (atualizaTab (atualizaTab t (l+1) (c+1) [Grama]) l (c+1) (idJogador id : newCelula), novaLista)
        | dir == d && d == S && movimentacaoValida t (l+1) c = (atualizaTab (atualizaTab t (l+1) (c+1) [Grama]) (l+2) (c+1) (idJogador id : newCelula), novaLista)
        | dir == d && d == L && movimentacaoValida t l (c+1) = (atualizaTab (atualizaTab t (l+1) (c+1) [Grama]) (l+1) (c+2) (idJogador id : newCelula), novaLista)
        | dir == d && d == O && movimentacaoValida t l (c-1) = (atualizaTab (atualizaTab t (l+1) (c+1) [Grama]) (l+1) c (idJogador id  : newCelula), novaLista)
        | otherwise = (t, lj)

        where
                newCelula
                        -- Celula modificada sem o presente (pego pelo jogador)
                        | d == N && (head (acessaCelula t (l-1) c) == Arremesso || head (acessaCelula t (l-1) c) == Patins || head (acessaCelula t (l-1) c) == Fogo) = drop 1 (acessaCelula t (l-1) c)
                        | d == S && (head (acessaCelula t (l+1) c) == Arremesso || head (acessaCelula t (l+1) c) == Patins || head (acessaCelula t (l+1) c) == Fogo) = drop 1 (acessaCelula t (l+1) c)
                        | d == L && (head (acessaCelula t l (c+1)) == Arremesso || head (acessaCelula t l (c+1)) == Patins || head (acessaCelula t l (c+1)) == Fogo) = drop 1 (acessaCelula t l (c+1))
                        | d == O && (head (acessaCelula t l (c-1)) == Arremesso || head (acessaCelula t l (c-1)) == Patins || head (acessaCelula t l (c-1)) == Fogo) = drop 1 (acessaCelula t l (c-1))
                        -- Celula se mantem
                        | d == N && head (acessaCelula t (l-1) c) == Grama = acessaCelula t (l-1) c
                        | d == S && head (acessaCelula t (l+1) c) == Grama = acessaCelula t (l+1) c
                        | d == L && head (acessaCelula t l (c+1)) == Grama = acessaCelula t l (c+1)
                        | d == O && head (acessaCelula t l (c-1)) == Grama = acessaCelula t l (c-1)

                novaLista
                        -- Att direcao 
                        | dir /= d =  take (id-1) lj ++ [atualizaDirecaoJogador (id, (l, c), dir, ((Patins, p), (Fogo, f), (Arremesso, a))) d] ++ drop id lj
                        -- Att posicao e presentes
                        | d == N && (head (acessaCelula t (l-1) c) == Arremesso || head (acessaCelula t (l-1) c) == Patins || head (acessaCelula t (l-1) c) == Fogo) = take (id-1) lj ++ [atualizaPresenteJogador (atualizaPosicaoJogador (id, (l, c), dir, ((Patins, p), (Fogo, f), (Arremesso, a)))  d) (head (acessaCelula t (l-1) c)) ] ++ drop id lj
                        | d == S && (head (acessaCelula t (l+1) c) == Arremesso || head (acessaCelula t (l+1) c) == Patins || head (acessaCelula t (l+1) c) == Fogo) = take (id-1) lj ++ [atualizaPresenteJogador (atualizaPosicaoJogador (id, (l, c), dir, ((Patins, p), (Fogo, f), (Arremesso, a)))  d) (head (acessaCelula t (l+1) c)) ] ++ drop id lj
                        | d == L && (head (acessaCelula t l (c+1)) == Arremesso || head (acessaCelula t l (c+1)) == Patins || head (acessaCelula t l (c+1)) == Fogo) = take (id-1) lj ++ [atualizaPresenteJogador (atualizaPosicaoJogador (id, (l, c), dir, ((Patins, p), (Fogo, f), (Arremesso, a)))  d) (head (acessaCelula t l (c+1))) ] ++ drop id lj
                        | d == O && (head (acessaCelula t l (c-1)) == Arremesso || head (acessaCelula t l (c-1)) == Patins || head (acessaCelula t l (c-1)) == Fogo) = take (id-1) lj ++ [atualizaPresenteJogador (atualizaPosicaoJogador (id, (l, c), dir, ((Patins, p), (Fogo, f), (Arremesso, a)))  d) (head (acessaCelula t l (c-1))) ] ++ drop id lj
                        -- Att posicao 
                        | d == N && (head (acessaCelula t (l-1) c) == Grama) = take (id-1) lj ++ [atualizaPosicaoJogador (id, (l, c), dir, ((Patins, p), (Fogo, f), (Arremesso, a))) d] ++ drop id lj
                        | d == S && (head (acessaCelula t (l+1) c) == Grama) = take (id-1) lj ++ [atualizaPosicaoJogador (id, (l, c), dir, ((Patins, p), (Fogo, f), (Arremesso, a))) d] ++ drop id lj
                        | d == L && (head (acessaCelula t l (c+1)) == Grama) = take (id-1) lj ++ [atualizaPosicaoJogador (id, (l, c), dir, ((Patins, p), (Fogo, f), (Arremesso, a))) d] ++ drop id lj
                        | d == O && (head (acessaCelula t l (c-1)) == Grama) = take (id-1) lj ++ [atualizaPosicaoJogador (id, (l, c), dir, ((Patins, p), (Fogo, f), (Arremesso, a))) d] ++ drop id lj

atualizaDirecaoJogador :: Jogador -> Direcao -> Jogador
atualizaDirecaoJogador (id, (l, c), dir, ((Patins, p), (Fogo, f), (Arremesso, a))) d
                        = (id, (l, c), d, ((Patins, p), (Fogo, f), (Arremesso, a)))


atualizaPresenteJogador :: Jogador -> Item -> Jogador
atualizaPresenteJogador (id, (l, c), dir, ((Patins, p), (Fogo, f), (Arremesso, a))) i
                | i == Patins = (id, (l, c), dir, ((Patins, p+1), (Fogo, f), (Arremesso, a)))
                | i == Fogo = (id, (l, c), dir, ((Patins, p), (Fogo, f+1), (Arremesso, a)))
                | i == Arremesso = (id, (l, c), dir, ((Patins, p), (Fogo, f), (Arremesso, a+1)))

atualizaPosicaoJogador :: Jogador -> Direcao -> Jogador
atualizaPosicaoJogador (id, (l, c), dir, ((Patins, p), (Fogo, f), (Arremesso, a))) d
                | d == N = (id, (l-1, c), dir, ((Patins, p), (Fogo, f), (Arremesso, a)))
                | d == S = (id, (l+1, c), dir, ((Patins, p), (Fogo, f), (Arremesso, a)))
                | d == L = (id, (l, c+1), dir, ((Patins, p), (Fogo, f), (Arremesso, a)))
                | d == O = (id, (l, c-1), dir, ((Patins, p), (Fogo, f), (Arremesso, a)))


-- ATUALIZAÇÃO DE TABULEIRO

-- Atualiza uma célula, recebendo linha, coluna e célula nova, e retorna o tabuleiro atualizado
atualizaTab :: Tabuleiro -> Int -> Int -> Celula -> Tabuleiro
atualizaTab tab l c cel = take (l-1) tab ++ [linhaAtualizada] ++ drop l tab
   where
      linhaAtualizada = atualizaCelula (tab !! (l-1)) c cel

--Atualiza a célula em uma linha e retorna a linha
atualizaCelula :: Linha -> Int -> Celula -> Linha
atualizaCelula linha c atual = take (c-1) linha ++ [atual] ++ drop c linha


colocarBomba :: Tabuleiro -> Jogador -> [Jogador] -> (Tabuleiro, [Jogador])
colocarBomba t (id, (l, c), dir, ((Patins, p), (Fogo, f), (Arremesso, a))) lj
                        | a > 0 = (newTab, novaLista)
                        where
                                novaLista = take (id-1) lj ++ [(id, (l, c), dir, ((Patins, p), (Fogo, f), (Arremesso, 0)))] ++ drop id lj
                                newTab = efetuarExplosoes t dir l c a

-- Explode de acordo com o numero de arremessos do jogador 
explodirCelula :: Tabuleiro -> Direcao -> Int -> Int -> Int -> (Tabuleiro, Int)
explodirCelula t d l c a

                | d == N && head (acessaCelula t (l-a) c) == Parede = (atualizaTab t l (c+1) (drop 1 (acessaCelula t (l-a) c)), a-1)
                | d == S && head (acessaCelula t (l+a) c) == Parede = (atualizaTab t (l+(a+1)) (c+1) (drop 1 (acessaCelula t (l+a) c)), a-1)
                | d == L && head (acessaCelula t l (c+a)) == Parede = (atualizaTab t (l+1) (c+(a+1)) (drop 1 (acessaCelula t l (c+a))), a-1)
                | d == O && head (acessaCelula t l (c-a)) == Parede = (atualizaTab t (l+1) ((c+1)-a) (drop 1 (acessaCelula t l (c-a))), a-1)

                | d == N && head (acessaCelula t (l-a) c) == Patins = (atualizaTab t l (c+1) (drop 1 (acessaCelula t (l-a) c)), a-1)
                | d == S && head (acessaCelula t (l+a) c) == Patins = (atualizaTab t (l+(a+1)) (c+1) (drop 1 (acessaCelula t (l+a) c)), a-1)
                | d == L && head (acessaCelula t l (c+a)) == Patins = (atualizaTab t (l+1) (c+(a+1)) (drop 1 (acessaCelula t l (c+a))), a-1)
                | d == O && head (acessaCelula t l (c-a)) == Patins = (atualizaTab t (l+1) ((c+1)-a) (drop 1 (acessaCelula t l (c-a))), a-1)

                | d == N && head (acessaCelula t (l-a) c) == Fogo = (atualizaTab t l (c+1) (drop 1 (acessaCelula t (l-a) c)), a-1)
                | d == S && head (acessaCelula t (l+a) c) == Fogo = (atualizaTab t (l+(a+1)) (c+1) (drop 1 (acessaCelula t (l+a) c)), a-1)
                | d == L && head (acessaCelula t l (c+a)) == Fogo = (atualizaTab t (l+1) (c+(a+1)) (drop 1 (acessaCelula t l (c+a))), a-1)
                | d == O && head (acessaCelula t l (c-a)) == Fogo = (atualizaTab t (l+1) ((c+1)-a) (drop 1 (acessaCelula t l (c-a))), a-1)

                | d == N && head (acessaCelula t (l-a) c) == Arremesso = (atualizaTab t l (c+1) (drop 1 (acessaCelula t (l-a) c)), a-1)
                | d == S && head (acessaCelula t (l+a) c) == Arremesso = (atualizaTab t (l+(a+1)) (c+1) (drop 1 (acessaCelula t (l+a) c)), a-1)
                | d == L && head (acessaCelula t l (c+a)) == Arremesso = (atualizaTab t (l+1) (c+(a+1)) (drop 1 (acessaCelula t l (c+a))), a-1)
                | d == O && head (acessaCelula t l (c-a)) == Arremesso = (atualizaTab t (l+1) ((c+1)-a) (drop 1 (acessaCelula t l (c-a))), a-1)

                | d == N && (head (acessaCelula t (l-a) c) == Jogador1 || head (acessaCelula t (l-a) c) == Jogador2 || head (acessaCelula t (l-a) c) == Jogador3) = (atualizaTab t l (c+1) (drop 1 (acessaCelula t (l-a) c)), a-1)
                | d == S && (head (acessaCelula t (l+a) c) == Jogador1 || head (acessaCelula t (l+a) c) == Jogador2 || head (acessaCelula t (l+a) c) == Jogador3) = (atualizaTab t (l+(a+1)) (c+1) (drop 1 (acessaCelula t (l+a) c)), a-1)
                | d == L && (head (acessaCelula t l (c+a)) == Jogador1 || head (acessaCelula t l (c+a)) == Jogador2 || head (acessaCelula t l (c+a)) == Jogador3) = (atualizaTab t (l+1) (c+(a+1)) (drop 1 (acessaCelula t l (c+a))), a-1)
                | d == O && (head (acessaCelula t l (c-a)) == Jogador1 || head (acessaCelula t l (c-a)) == Jogador2 || head (acessaCelula t l (c-a)) == Jogador3) = (atualizaTab t (l+1) ((c+1)-a) (drop 1 (acessaCelula t l (c-a))), a-1)
                | otherwise = (t, a-1)

efetuarExplosoes :: Tabuleiro -> Direcao -> Int -> Int -> Int -> Tabuleiro
efetuarExplosoes t d l c 0 = t
efetuarExplosoes t d l c a = efetuarExplosoes (fst aux) d l c (snd aux)
                where
                        aux = explodirCelula t d l c a

