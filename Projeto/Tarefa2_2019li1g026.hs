-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa2_2019li1g026 where

import LI11920
import Data.List


-- * Testes

-- | Testes unitários da Tarefa 2.
--
-- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).
testesT2 :: [(Int,Jogada,Estado)]
testesT2 = [
               
                (1, Movimenta C, Estado [[Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0,Recta Lama 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Lama 0 2,Rampa Lama 2 0]] [Jogador 0 (1.8) 1 1 (Chao False), Jogador 1 0 1 1 (Chao True)]),
                (1, Movimenta B, Estado [[Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Terra 0,Recta Relva 0],[Recta Terra 0,Rampa Terra 0 2,Rampa Terra 2 0,Recta Terra 0,Rampa Relva 0 2],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Rampa Terra 0 1]] [Jogador 0 1.8 1 2 (Chao False), Jogador 1 3 1.2 0 (Chao True), Jogador 2 0 1 0 (Chao True)]),
                (2, Dispara, Estado [[Recta Terra 0,Recta Boost 0,Recta Terra 0,Recta Relva 0,Recta Relva 0],[Recta Terra 0,Rampa Terra 0 2,Rampa Relva 2 3,Recta Relva 3,Rampa Terra 3 1],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Terra 2 1],[Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Terra 0,Recta Terra 0]] [Jogador 0 2 1.9 0 (Chao True), Jogador 1 3 1.2 2 (Chao True), Jogador 2 1 1.2 0 (Chao False),Jogador 3 3 0.8 2 (Ar 2 60 0)]),
                (0, Acelera, Estado [[Recta Terra 0,Recta Relva 0,Recta Relva 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0]] [Jogador 0 1.2 1 0 (Chao False), Jogador 1 0.3 2 1 (Chao True)]),
                (1, Movimenta E, Estado [[Recta Terra 0,Recta Boost 0,Recta Terra 0,Recta Relva 0,Recta Relva 0],[Recta Terra 0,Rampa Terra 0 2,Rampa Relva 2 3,Recta Relva 3,Rampa Terra 3 1],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Terra 2 1]] [Jogador 0 3 1.2 2 (Chao True), Jogador 1 1 1.2 0 (Chao False),Jogador 2 3 0.8 2 (Ar 2 60 0)]),
                (0, Movimenta D, Estado [[Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Terra 0,Rampa Terra 0 1],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0]] [Jogador 0 3.1 1 0 (Ar 1 60 0),Jogador 1 2.4 3 0 (Chao True)]),
                (3, Movimenta C, Estado [[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Boost 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 1],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Relva 2 4],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Rampa Terra 1 3],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0]] [Jogador 0 1.8 1 2 (Morto 1.0), Jogador 1 3 1.2 0 (Chao True), Jogador 2 0 1 0 (Chao True), Jogador 3 2.2 1 0 (Chao True)]),
                (1, Dispara, Estado [[Recta Terra 0,Recta Terra 0,Recta Terra 0],[Recta Terra 0,Recta Relva 0,Recta Terra 0]] [Jogador 0 2 1 1 (Chao False), Jogador 1 2.1 2 3 (Ar 1 20 0)]),
                (1, Desacelera, Estado [[Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Terra 0,Rampa Terra 0 1],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0]] [Jogador 0 3.1 1 0 (Ar 1 60 0),Jogador 1 2.4 3 0 (Chao True)]),
                (2, Movimenta D,Estado [[Recta Terra 0,Recta Boost 0,Recta Terra 0,Recta Relva 0,Recta Relva 0],[Recta Terra 0,Rampa Terra 0 2,Rampa Relva 2 3,Recta Relva 3,Rampa Terra 3 1],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Terra 2 1]] [Jogador 0 3 1.2 2 (Chao True), Jogador 1 1 1.2 0 (Chao False),Jogador 2 3 0.8 2 (Ar 2 60 0)]),
                (1, Movimenta E, Estado [[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Boost 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 1],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Relva 2 4],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Rampa Terra 1 3],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0]] [Jogador 0 1.8 1 2 (Morto 1.0), Jogador 1 3 1.2 0 (Chao True), Jogador 2 0 1 0 (Chao True), Jogador 3 3.2 1 0 (Chao True)]),
                (0, Dispara, Estado [[Recta Terra 0,Recta Terra 0,Recta Terra 0],[Recta Terra 0,Recta Relva 0,Recta Terra 0]] [Jogador 0 2 1 1 (Morto 1.0), Jogador 1 2.1 2 3 (Ar 1 20 0)]),
                (2, Acelera, Estado [[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Boost 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 1],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Relva 2 4],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Rampa Terra 1 3],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0]] [Jogador 0 1.8 1 2 (Morto 1.0), Jogador 1 3 1.2 0 (Chao True), Jogador 2 0 1 0 (Chao True), Jogador 3 3.2 1 0 (Chao True)]),
                (3, Movimenta C,Estado [[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Boost 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 1],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Relva 2 4],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Rampa Terra 1 3],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0]] [Jogador 0 1.8 1 2 (Morto 1.0), Jogador 1 3 1.2 0 (Chao True), Jogador 2 0 1 0 (Ar 2 0 0), Jogador 3 3.2 1 0 (Chao True)]),
                (0, Movimenta B,Estado [[Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Terra 0,Recta Relva 0,Rampa Relva 0 2,Rampa Terra 2 0,Recta Terra 0],[Recta Terra 0,Rampa Relva 0 2,Rampa Relva 2 0,Recta Relva 0,Recta Boost 0,Rampa Terra 0 1,Rampa Lama 1 2,Recta Lama 2]] [Jogador 0 5.6 1 0 (Chao False),Jogador 0 2.2 2 1 (Chao True)]),
                (1, Acelera, Estado [[Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Terra 0,Rampa Terra 0 1],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0]] [Jogador 0 3.1 1 0 (Ar 1 60 0),Jogador 1 2.4 3 0 (Chao True)]),
                (0, Desacelera, Estado [[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Boost 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 1],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Relva 2 4],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Rampa Terra 1 3],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0]] [Jogador 0 1.8 1 2 (Morto 1.0), Jogador 1 3 1.2 0 (Chao True), Jogador 2 0 1 0 (Chao True), Jogador 3 3.2 1 0 (Chao True)]),
                (1, Dispara, Estado [[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Boost 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 1],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Relva 2 4],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Rampa Terra 1 3],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0]] [Jogador 0 1.8 1 2 (Ar 2 45 0), Jogador 1 3 1.2 0 (Chao True), Jogador 2 0 1 0 (Chao True), Jogador 3 3.2 1 0 (Chao True)]),
                (2, Movimenta D, Estado [[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Boost 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 1],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Relva 2 4],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Rampa Terra 1 3],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0]] [Jogador 0 1.8 1 2 (Morto 1.0), Jogador 1 3 1.2 0 (Chao True), Jogador 2 0 1 0 (Chao True), Jogador 3 3.2 1 0 (Chao True)]),
                (0, Movimenta E, Estado [[Recta Terra 0,Recta Boost 0,Recta Terra 0,Recta Relva 0,Recta Relva 0],[Recta Terra 0,Rampa Terra 0 2,Rampa Relva 2 3,Recta Relva 3,Rampa Terra 3 1],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Terra 2 1]] [Jogador 0 3 1.2 2 (Chao True), Jogador 1 1 1.2 0 (Chao False),Jogador 2 3 0.8 2 (Ar 2 60 0)]),
                (3, Acelera, Estado [[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Boost 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 1],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Relva 2 4],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Rampa Terra 1 3],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0]] [Jogador 0 1.8 1 2 (Morto 1.0), Jogador 1 3 1.2 0 (Chao True), Jogador 2 0 1 0 (Ar 2 0 0), Jogador 3 3.2 1 0 (Chao True)]),
                (1, Desacelera, Estado [[Recta Terra 0,Recta Boost 0,Recta Terra 0,Recta Relva 0,Recta Relva 0],[Recta Terra 0,Rampa Terra 0 2,Rampa Relva 2 3,Recta Relva 3,Rampa Terra 3 1],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Terra 2 1]] [Jogador 0 3 1.2 2 (Chao True), Jogador 1 1 1.4 0 (Chao True),Jogador 2 3 2.3 2 (Ar 2 40 0)]),
                (2, Movimenta B, Estado [[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Boost 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 1],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Relva 2 4],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Rampa Terra 1 3],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0]] [Jogador 0 1.8 1 2 (Morto 1.0), Jogador 1 3 1.2 0 (Chao True), Jogador 2 0 1 0 (Ar 2 0 0), Jogador 3 3.2 1 0 (Chao True)]),
                (2, Dispara, Estado [[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Boost 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 1],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Relva 2 4],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Rampa Terra 1 3],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0]] [Jogador 0 1.8 1 2 (Morto 1.0), Jogador 1 3 1.2 0 (Morto 1.0), Jogador 2 0 1 0 (Chao True), Jogador 3 3.2 1 0 (Chao True)])
           ]

-- * Funções principais da Tarefa 2.

-- | Efetua uma jogada.
jogada :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
       -> Jogada -- ^ A 'Jogada' a efetuar.
       -> Estado -- ^ O 'Estado' anterior.
       -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.
jogada n (Movimenta C) (Estado m (x:xs)) = (Estado m (atualizaJogadorLista n (movimenta C m (indPlayer n (x:xs))) (x:xs)))
jogada n (Movimenta B) (Estado m (x:xs)) = (Estado m (atualizaJogadorLista n (movimenta B m (indPlayer n (x:xs))) (x:xs)))
jogada n (Movimenta D) (Estado m (x:xs)) = (Estado m (atualizaJogadorLista n (movimenta D m (indPlayer n (x:xs))) (x:xs)))
jogada n (Movimenta E) (Estado m (x:xs)) = (Estado m (atualizaJogadorLista n (movimenta E m (indPlayer n (x:xs))) (x:xs)))
jogada n (Acelera) (Estado m (x:xs)) = (Estado m (atualizaJogadorLista n (paraoacelera n (x:xs)) (x:xs)))
jogada n (Desacelera) (Estado m (x:xs)) = (Estado m (atualizaJogadorLista n (paraodesacelera n (x:xs)) (x:xs)))
jogada n (Dispara) (Estado m (x:xs)) = dispara n (x:xs) m
                                                  
-- *Funções necessárias para o funcionamento da função "movimenta"

-- | Função que dado o índice do jogador e a lista de jogadores, retorna o jogador correspondente ao índice
indPlayer :: Int -> [Jogador] -> Jogador
indPlayer 0 (x:xs) = x
indPlayer n (x:xs) = indPlayer (n-1) xs

-- | Função que retorna a diferença de altura entre duas peças
difAltura :: Peca -> Peca -> Double
difAltura (Recta tipo h) (Recta tipo1 h1) = abs((daAlturaf(Recta tipo h))-(daAlturaI(Recta tipo1 h1)))
difAltura (Recta tipo h) (Rampa tipo1 h1 h2) = abs((daAlturaf(Recta tipo h))-(daAlturaI(Rampa tipo1 h1 h2))) 
difAltura (Rampa tipo h1 h2) (Rampa tipo1 h3 h4) = abs((daAlturaf(Rampa tipo h1 h2))-(daAlturaI(Rampa tipo1 h3 h4)))
difAltura (Rampa tipo h1 h2) (Recta tipo1 h) = abs((daAlturaf(Rampa tipo h1 h2))-(daAlturaI(Recta tipo1 h1)))


-- | Função que verifica se as alturas entre as duas pistas são válidas para o jogador trocar de pista
podeTransitar :: Peca -> Peca -> Bool 
podeTransitar (Recta tipo h) (Recta tipo1 h1)  =  if ((difAltura (Recta tipo h) (Recta tipo1 h1))<=0.2) then True else False
podeTransitar (Recta tipo h) (Rampa tipo1 h1 h2) =  if ((difAltura (Recta tipo h) (Rampa tipo1 h1 h2))<=0.2) then True else False
podeTransitar (Rampa tipo h1 h2) (Rampa tipo1 h3 h4) = if ((difAltura (Rampa tipo h1 h2) (Rampa tipo1 h3 h4))<=0.2) then True else False
podeTransitar (Rampa tipo h1 h2) (Recta tipo1 h) =   if ((difAltura (Rampa tipo h1 h2) (Recta tipo1 h))<=0.2) then True else False


-- | Funçao que determina a pista que está em cima, ou seja, a pista em cima da qual o jogador se encontra
pistaCima :: Jogador -> Mapa -> Pista
pistaCima (Jogador p d v c e) m |p<=0 = dropMap (takeMap 1 m)
                                |otherwise = dropMap (takeMap (fromIntegral(p)) m)

-- | Funçao que determina a Peca de cima da qual o jogador se encontra
pecaCima :: Jogador -> Mapa -> Peca
pecaCima (Jogador p d v c e) m = (pecInd (Jogador p d v c e) (pistaCima (Jogador p d v c e) m))

-- | Funçao que determina o indice da peça em estudo(em double), ou seja, a peça onde o jogador está
indPeca :: Jogador -> Double
indPeca (Jogador p d v c e) = d

-- | Funçao que determina a pista que está em baixo, ou seja, a pista em baixo da qual o jogador se encontra
pistaBaixo :: Jogador -> Mapa -> Pista
pistaBaixo (Jogador p d v c e) m |p >= ((length m)-1) = dropMap  m
                                 |p < ((length m)-1) = (dropMap (takeMap (fromIntegral(p+2)) m))

-- | Funçao que determina a Peca em baixo da qual o jogador se encontra
pecaBaixo :: Jogador -> Mapa -> Peca
pecaBaixo (Jogador p d v c e) m = (pecInd (Jogador p d v c e) (pistaBaixo (Jogador p d v c e) m))

-- | Funçao que determina em que peça o jogador está
wherePlayer :: Jogador-> Mapa -> Peca
wherePlayer (Jogador p d v c e) m = pecInd (Jogador p d v c e) (dropMap(takeMap (fromIntegral(p+1)) m))

-- | Funçoes necessarias para determinar as peças que estao em cima e em baixo da qual o jogador está
-- | Função que dado um mapa retorna a última pista desse mapa
dropMap :: Mapa -> Pista
dropMap  [[]]=[]
dropMap  l = last l

-- | Função que dada uma pista, retorna a última peça dessa pista
dropPista :: Pista -> Peca
dropPista l = last l

-- | Função que retorna um mapa com as primeiras n pistas do mapa
takeMap :: Double -> Mapa -> Mapa
takeMap n [[]] = [[]]
takeMap n (x:xs) |n<1 = [[]]
                 |n==1 = [x]
                 |n>1 = concatenaLL x (takeMap (n-1) xs)

-- | Função que retorna uma pista com as primeiras n peças da pista
takePista :: Double -> Pista -> Pista
takePista n [] = []
takePista n (x:xs) |n<1 =[]
                   |n==1 = [x]
                   |n>1 = concatenaPL x (takePista (n-1) xs)

-- | Função que dado um jogador e a sua respetiva pista, indica em que peça ele se encontra
pecInd :: Jogador -> Pista -> Peca
pecInd (Jogador p d v c e) l = dropPista (takePista ((indPeca (Jogador p d v c e))+1) l)

-- | Função que concatena uma pista a um mapa retornando um mapa com essa pista no início
concatenaLL :: Pista -> Mapa -> Mapa
concatenaLL p [[]] = [p]
concatenaLL p (x:xs) = (p:x:xs)

-- | Função que concatena uma peça a uma pista retornando uma pista com essa peça no início
concatenaPL :: Peca -> Pista -> Pista
concatenaPL p [] = [p]
concatenaPL p l = (p:l)

-- | Função que retorna o número de pistas de um mapa (em double)
lengthLL :: Mapa -> Double
lengthLL [[]]= fromIntegral(0)
lengthLL (x:xs)= (realToFrac (length(x:xs)))

-- | Funções necessarias para determinar a inclinaçao 
-- | Função que aumenta a inclinação de um jogador que está no ar
aumentaInc :: Jogador -> Jogador
aumentaInc (Jogador p d v c (Ar alt inc grav)) | ((inc+15) < 90) = (Jogador p d v c (Ar alt (inc+15) grav))
                                               | ((inc+15) >= 90) = (Jogador p d v c (Ar alt 90 grav))

-- | Função que diminui a inclinação de um jogador que está no ar
diminuiInc :: Jogador -> Jogador
diminuiInc (Jogador p d v c (Ar alt inc grav)) | ((inc-15)>(-90)) = (Jogador p d v c (Ar alt (inc-15) grav))
                                               | ((inc-15)<=(-90)) = (Jogador p d v c (Ar alt (-90) grav))

-- | Testa se a altura inicial da peça de baixo do jogador é menor que a altura final onde ele se encontra
altMenorB :: Jogador -> Mapa -> Bool
altMenorB (Jogador p d v c e) m = if ((daAlturaf(wherePlayer(Jogador p d v c e) m))>(daAlturaI(pecaBaixo(Jogador p d v c e) m))) then True else False

-- | Função que dá a altura final de uma peça
daAlturaf :: Peca -> Double
daAlturaf (Recta tipo h) = fromIntegral(h)
daAlturaf (Rampa tipo h1 h2) = fromIntegral(h2)

-- | Função que dá a altura inicial de uma peça
daAlturaI :: Peca -> Double
daAlturaI (Recta tipo h) = fromIntegral(h)
daAlturaI (Rampa tipo h1 h2) = fromIntegral(h1)

-- | Testa se a altura inicial da peça de cima do jogador é menor que a altura final onde ele se encontra
altMenorC :: Jogador -> Mapa -> Bool
altMenorC (Jogador p d v c e) m = if ((daAlturaf (wherePlayer(Jogador p d v c e) m)) > (daAlturaI (pecaCima(Jogador p d v c e) m))) then True else False

-- | Função que retorna a inclinação entre duas peças
incPecAnt :: Peca -> Double
incPecAnt (Recta tipo h) = 0
incPecAnt (Rampa tipo h1 h2) = (inclinacao h1 h2)

-- | Função que retorna a inclinação entre duas alturas
inclinacao :: Int -> Int -> Double
inclinacao h1 h2 = (atan (fromIntegral(h2-h1))) * 57.29577951308230876798155

-- * Função fundamental para a opção "Movimenta"
movimenta :: Direcao -> Mapa -> Jogador -> Jogador 
movimenta C m (Jogador p d v c e) |((fromIntegral(p))>0) && (e==(Chao True)) && ((podeTransitar (wherePlayer (Jogador p d v c e) m) (pecaCima (Jogador p d v c e) m))==True) = (Jogador (p-1) d v c (Chao True))
                                  |((fromIntegral(p))>0) && (e==(Chao False)) && ((podeTransitar (wherePlayer (Jogador p d v c e) m) (pecaCima (Jogador p d v c e) m))==True) = (Jogador (p-1) d v c (Chao False))
                                  |((fromIntegral(p))>0) && (e==(Chao True)) && ((podeTransitar (wherePlayer (Jogador p d v c e) m) (pecaCima (Jogador p d v c e) m))==False) && ((altMenorC (Jogador p d v c e) m)==False) = (Jogador p d v c (Morto 1.0))
                                  |((fromIntegral(p))>0) && (e==(Chao False)) && ((podeTransitar (wherePlayer (Jogador p d v c e) m) (pecaCima (Jogador p d v c e) m))==False) && ((altMenorC (Jogador p d v c e) m)==False) = (Jogador p d v c (Morto 1.0))
                                  |((fromIntegral(p))>0) && (e==(Chao True)) && ((podeTransitar (wherePlayer (Jogador p d v c e) m) (pecaCima (Jogador p d v c e) m))==False) && ((altMenorC (Jogador p d v c e) m)==True) = (Jogador (p-1) d v c (Ar (daAlturaf(wherePlayer(Jogador p d v c e)m)) (incPecAnt(wherePlayer(Jogador p d v c e)m)) 0))
                                  |((fromIntegral(p))>0) && (e==(Chao False)) && ((podeTransitar (wherePlayer (Jogador p d v c e) m) (pecaCima (Jogador p d v c e) m))==False) && ((altMenorC (Jogador p d v c e) m)==True) = (Jogador (p-1) d v c (Ar (daAlturaf(wherePlayer(Jogador p d v c e)m)) (incPecAnt(wherePlayer(Jogador p d v c e)m)) 0))
                                  |otherwise = (Jogador p d v c e)
movimenta B m (Jogador p d v c e) |((fromIntegral(p))<((lengthLL m)-1)) && (e==(Chao True)) && ((podeTransitar (wherePlayer (Jogador p d v c e) m) (pecaBaixo (Jogador p d v c e) m))==True) = (Jogador (p+1) d v c (Chao True))
                                  |((fromIntegral(p))<((lengthLL m)-1)) && (e==(Chao False)) && ((podeTransitar (wherePlayer (Jogador p d v c e) m) (pecaBaixo (Jogador p d v c e) m))==True) = (Jogador (p+1) d v c (Chao False))
                                  |((fromIntegral(p))<((lengthLL m)-1)) && (e==(Chao True)) && ((podeTransitar (wherePlayer (Jogador p d v c e) m) (pecaBaixo (Jogador p d v c e) m))==False) && ((altMenorB (Jogador p d v c e) m)==False) = (Jogador p d 0 c (Morto 1.0))
                                  |((fromIntegral(p))<((lengthLL m)-1)) && (e==(Chao False)) && ((podeTransitar (wherePlayer (Jogador p d v c e) m) (pecaBaixo (Jogador p d v c e) m))==False) && ((altMenorB (Jogador p d v c e) m)==False) = (Jogador p d 0 c (Morto 1.0)) 
                                  |((fromIntegral(p))<((lengthLL m)-1)) && (e==(Chao True)) && ((podeTransitar (wherePlayer (Jogador p d v c e) m) (pecaBaixo (Jogador p d v c e) m))==False) && ((altMenorB (Jogador p d v c e) m)==True) = (Jogador (p+1) d v c (Ar (daAlturaf(wherePlayer(Jogador p d v c e)m)) (incPecAnt(wherePlayer(Jogador p d v c e)m)) 0))
                                  |((fromIntegral(p))<((lengthLL m)-1)) && (e==(Chao False)) && ((podeTransitar (wherePlayer (Jogador p d v c e) m) (pecaBaixo (Jogador p d v c e) m))==False) && ((altMenorB (Jogador p d v c e) m)==True) = (Jogador (p+1) d v c (Ar (daAlturaf(wherePlayer(Jogador p d v c e)m)) (incPecAnt(wherePlayer(Jogador p d v c e)m)) 0))
                                  |otherwise = (Jogador p d v c e)
movimenta E m (Jogador p d v c (Ar a i g))  = aumentaInc (Jogador p d v c (Ar a i g))
movimenta E m (Jogador p d v c (Chao True)) = (Jogador p d v c (Chao True))
movimenta E m (Jogador p d v c (Chao False)) = (Jogador p d v c (Chao False))
movimenta E m (Jogador p d v c (Morto x)) = (Jogador p d v c (Morto x))
movimenta D m (Jogador p d v c (Ar a i g)) = diminuiInc (Jogador p d v c (Ar a i g))
movimenta D m (Jogador p d v c (Chao True)) = (Jogador p d v c (Chao True))
movimenta D m (Jogador p d v c (Chao False)) = (Jogador p d v c (Chao False))                                 
movimenta D m (Jogador p d v c (Morto x)) = (Jogador p d v c (Morto x))


-- * Funções essenciais para as opções "Acelera" e "Desacelera",respetivamente.
-- | Função que dado o jogador retorna-o como se tivesse acionado a opção "Acelera"
permiteAcelerar :: Jogador -> Jogador
permiteAcelerar (Jogador p d v c e) | (e==(Chao False)) = (Jogador p d v c (Chao True))
                                    | otherwise = (Jogador p d v c e)

-- | Função que dado o jogador retorna-o como se tivesse acionado a opção "Desacelera"
permiteDesacelerar :: Jogador -> Jogador
permiteDesacelerar (Jogador p d v c e) | (e==(Chao True)) = (Jogador p d v c (Chao False))
                                       | otherwise = (Jogador p d v c e)


-- | Função que dada o índice e a lista de jogadores, retorna o jogador pretendido com a opção "Acelera" acionada
paraoacelera :: Int -> [Jogador] -> Jogador
paraoacelera n (x:xs)  = permiteAcelerar (indPlayer n (x:xs))

-- | Função que dada o índice e a lista de jogadores, retorna o jogador pretendido com a opção "Desacelera" acionada
paraodesacelera :: Int -> [Jogador] -> Jogador
paraodesacelera n (x:xs) = permiteDesacelerar (indPlayer n (x:xs))

-- * Funções auxiliares essenciais para a opção "Dispara" 
-- | Função que retorna a peça anterior,na mesma pista, da qual o jogador se encontra
pecaAnt :: Jogador -> Mapa -> Peca
pecaAnt (Jogador p d v c e) mp =  (pecAntInd (Jogador p d v c e) (dropMap(takeMap (fromIntegral(p)) mp)))

-- | Função que dado um jogador e a sua respetiva pista, indica em que peça anterior, da mesma pista, que ele se encontra
pecAntInd :: Jogador -> Pista -> Peca
pecAntInd (Jogador p d v c e) l = dropPista (takePista (indPeca (Jogador p d v c e)) l)

-- | Função que retorna a quantidade de munições de cola de um jogador
colaPlayer :: Jogador -> Int
colaPlayer (Jogador p d v c e) = c

-- | Função que dado um índice e uma lista de jogadores, retira a munição ao jogador pretendido, retornando-o
tiraMunicao :: Int -> [Jogador] -> Jogador
tiraMunicao n l = subtraiMunicao (indPlayer n l)

-- | Função que tira a munição a um jogador
subtraiMunicao :: Jogador -> Jogador
subtraiMunicao (Jogador p d v c e) = (Jogador p d v (c-1) e)

-- | Função que troca o piso de uma peça para Cola
mudaPeca :: Peca -> Peca
mudaPeca (Rampa piso h1 h2) = (Rampa Cola h1 h2)
mudaPeca (Recta piso h) = (Recta Cola h)

-- | Função que coloca piso Cola na peça anterior à qual o jogador se encontra
colocaPiso :: Jogador -> Mapa -> Peca
colocaPiso j m = mudaPeca(pecaAnt j m)

-- | Função que dado um índice e uma lista de jogadores, indica a posição no mapa do jogador pretendido
posicaoMapa :: Int -> [Jogador] -> (Int,Int)
posicaoMapa n (x:xs) = daPosicao (indPlayer n (x:xs))

-- | Função que retorna a posição de um jogador no mapa
daPosicao :: Jogador -> (Int,Int)
daPosicao (Jogador p d v c e) |(d<1) = (p,0)
                              |(d>=1) = (p,daIndColuna (Jogador p d v c e))

-- | Função que retorna a posição da peca anterior
daPosicaoAnt :: (Int,Int) -> (Int,Int)
daPosicaoAnt (x,y) | (x>=0 && y>=1) = (x,(y-1))
                   | otherwise = (0,0)

-- | Função que retorna em que coluna do mapa o jogador se encontra
daIndColuna :: Jogador -> Int
daIndColuna (Jogador p d v c e) | (d<1) = 0
                                | (d>=1) =1+daIndColuna (Jogador p (d-1) v c e)

-- | Função que dada uma posição, uma peça e um mapa retorna um mapa atualizando o mapa com essa peça na posição pretendida
atualizaPecaMapa :: (Int,Int) -> Peca -> Mapa -> Mapa
atualizaPecaMapa (n,m) e (x:xs) | (n > length(x:xs)) || (m > length x) = (x:xs)
                                | (n == 0) = (atualizaIndicePista m e x):xs
                                | otherwise = x:(atualizaPecaMapa (n-1, m) e xs) 

-- | Função que dada uma posição, uma peça e uma pista retorna uma pista atualizando a pista com essa peça na posição pretendida 
atualizaIndicePista :: Int -> Peca -> Pista -> Pista
atualizaIndicePista n e [] = []
atualizaIndicePista n e (x:xs) | (n == 0) = e:xs 
                               | otherwise = x:(atualizaIndicePista (n-1) e xs)

-- | Função que dada uma posição, um jogador e uma lista de jogadores, retorna uma lista de jogadores atualizada com o jogador dado na posição dada
atualizaJogadorLista :: Int -> Jogador -> [Jogador] -> [Jogador]
atualizaJogadorLista n j (x:xs) |(n==0) = j:xs
                                |otherwise = x:(atualizaJogadorLista (n-1) j xs) 

-- | Função que dado um índice e uma lista de jogadores, retorna a distância do jogador pretendido
daDistancia :: Int -> [Jogador] -> Double
daDistancia n (x:xs) = tiraDistancia (indPlayer n (x:xs))

-- | Função que dado um jogador, retorna a sua distância
tiraDistancia :: Jogador -> Double
tiraDistancia (Jogador p d v c e) = d

-- | Função que dado um índice e uma lista de jogadores, retorna o estado do jogador pretendido
daEstado :: Int -> [Jogador] -> EstadoJogador
daEstado n (x:xs) = tiraEstado (indPlayer n (x:xs))

-- | Função que dado um jogador, retorna o seu estado
tiraEstado :: Jogador -> EstadoJogador
tiraEstado (Jogador p d v c e) = e

-- | Função essencial para a opção "Dispara", retornando o estado suposto
dispara :: Int -> [Jogador] -> Mapa -> Estado
dispara n (x:xs) m | (((colaPlayer(indPlayer n (x:xs)))<= 0) || ((daDistancia n (x:xs))<=(fromIntegral(1)))) || (((daEstado n (x:xs))/= (Chao True)) && ((daEstado n (x:xs))/=(Chao False))) = (Estado m (x:xs))
                   | (((colaPlayer(indPlayer n (x:xs))) > 0) && ((daDistancia n (x:xs))>(fromIntegral(1)))) && (((daEstado n (x:xs))== (Chao True))||((daEstado n (x:xs))==(Chao False))) =  (Estado (atualizaPecaMapa (daPosicaoAnt(daPosicao (indPlayer n (x:xs)))) (colocaPiso (indPlayer n (x:xs)) m) m) (atualizaJogadorLista n (tiraMunicao n (x:xs)) (x:xs)))