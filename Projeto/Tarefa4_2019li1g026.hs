-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2019li1g026 where

import LI11920
import Tarefa0_2019li1g026

-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um par (/tempo/,/'Mapa'/,/'Jogador'/).
testesT4 :: [(Double,Mapa,Jogador)]
testesT4 = [
                (2, [[Recta Terra 0, Recta Terra 0, Recta Relva 0, Recta Lama 0]], Jogador 0 1.9 1 1 (Chao False)),
                (1, [[Recta Terra 0, Recta Lama 0, Rampa Relva 0 2, Recta Boost 2]], Jogador 0 1 3 0 (Chao True)),
                (1, [[Recta Terra 0, Recta Lama 0, Rampa Relva 0 2, Recta Boost 2]], Jogador 0 1 1 0 (Chao True)),
                (1, [[Recta Terra 0, Rampa Relva 0 3, Rampa Relva 3 0, Recta Terra 0]] , Jogador 0 2 1 1 (Ar 4 15 1)),
                (1, [[Recta Terra 0, Rampa Relva 0 3, Rampa Relva 3 0, Recta Terra 0]] , Jogador 0 2 2 1 (Ar 4 15 1)),
                (1, [[Recta Terra 0, Rampa Terra 0 2, Recta Relva 2, Rampa Terra 2 0]], Jogador 0 2 1 1 (Ar 3 15 1)),
                (1, [[Recta Terra 0, Rampa Relva 0 3, Rampa Relva 3 0, Recta Terra 0]] , Jogador 0 2 2 1 (Ar 4 0 1)),
                (1, [[Recta Terra 0, Rampa Terra 0 2, Recta Relva 2, Rampa Terra 2 0]], Jogador 0 2 1 1 (Ar 3 50 1)), 
                (1, [[Recta Terra 0, Rampa Terra 0 2, Rampa Relva 2 0, Rampa Terra 0 2, Recta Boost 2]], Jogador 0 2 2 1 (Ar 4 (-15) 1)),
                (1, [[Recta Terra 0, Rampa Terra 0 2, Recta Relva 2, Rampa Terra 2 0]], Jogador 0 2.4 1 1 (Ar 3 (-15) 1)),
                (0.2, [[Recta Terra 0, Recta Lama 0, Rampa Relva 0 2, Recta Boost 2]], Jogador 0 2.3 0.8 0 (Chao False))
           ]

-- * Funções principais da Tarefa 4.

-- | Avança o estado de um 'Jogador' um 'passo' em frente, durante um determinado período de tempo.
passo :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após um 'passo'.
passo t m j = move t m (acelera t m j)

-- | Altera a velocidade de um 'Jogador', durante um determinado período de tempo.
acelera :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após acelerar.
acelera t m j = arChao t m j 

-- | Altera a posição de 'Jogador', durante um determinado período de tempo.
move :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após se movimentar.
move t m j = qualMove t m j


-- * Acelera
-- | Faz corresponder o valor do atrito a cada peça, tendo em consideracao o tipo de piso.
obtemAtrito :: Peca -> Double  
obtemAtrito (Recta tipo _) | tipo == Relva = 0.75 
                           | tipo == Terra = 0.25 
                           | tipo == Lama = 1.50 
                           | tipo == Boost = (-0.50)
                           | tipo == Cola = 3.00 

obtemAtrito (Rampa tipo _ _) | tipo == Relva = 0.75 
                             | tipo == Terra = 0.25 
                             | tipo == Lama = 1.50 
                             | tipo == Boost = -0.50
                             | tipo == Cola = 3.00 

-- | Calcula o "accelMota" que vai ser necessario para a funcao 'atualizaVelocidade'.
accelMota :: Jogador -> Double  
accelMota (Jogador _ _ vel _ stat) | vel < 2 && stat == (Chao True) = 1 
                                   | otherwise = 0 

{- | Atualiza a velocidade de um jogador (quer esteja no ar ou no chao). -}
atualizaVelocidade :: Double   -- ^ O tempo decorrido
                   -> Mapa     -- ^ O Mapa onde esta a ocorrer o jogo
                   -> Jogador  -- ^ O estado anterior do 'Jogador'
                   -> Jogador  -- ^ O estado do 'Jogador' apos lhe ter sido atualizada a velocidade

atualizaVelocidade t m j@(Jogador pista dist vel cola (Chao estado)) | vel <= 0 = (Jogador pista dist 0.0 cola (Chao estado))
                                                                     | v <= 0 = (Jogador pista dist 0 cola (Chao estado))
                                                                     | otherwise = (Jogador pista dist v cola (Chao estado))
     where 
          v = (vel + ((accelMota j) - (obtemAtrito (encontraPecaMatriz (fromIntegral(pista), dist) m)) * vel) * t) 
        
-- | Atualiza a velocidade do jogador no ar.
atualizaVelocidadeAr :: Double   -- ^ O tempo decorrido.
                     -> Mapa     -- ^ O Mapa onde esta a ocorrer o jogo.
                     -> Jogador  -- ^ O estado anterior do 'Jogador'.
                     -> Jogador  -- ^ O estado do 'Jogador' apos lhe ter sido atualizada a velocidade.
atualizaVelocidadeAr t m j@(Jogador pista dist vel cola (Ar h inc g)) | vel < 0 = (Jogador pista dist 0 cola (Ar h inc grav))
                                                                      | otherwise = (Jogador pista dist vAR cola (Ar h inc grav))
                                                                      
          where
               vAR = (vel - (0.125 * vel * t))
               grav = (g + 1 * t)

-- | Atualiza a velocidade do jogador morto.               
atualizaVelocidadeMorto :: Double  -- ^ O tempo decorrido.
                        -> Mapa    -- ^ O Mapa onde esta a ocorrer o jogo.
                        -> Jogador -- ^ O estado anterior do 'Jogador'.
                        -> Jogador -- ^ O estado do 'Jogador' apos lhe ter sido atualizada a velocidade.
atualizaVelocidadeMorto t m j@(Jogador pista dist vel cola (Morto timeout)) = j

-- * Move 
-- | Funcao que determina o movimento do jogador quando este se encontra morto.
moveMorto :: Double   -- ^ O tempo decorrido.
          -> Mapa     -- ^ O Mapa onde esta a ocorrer o jogo.
          -> Jogador  -- ^ O estado anterior do 'Jogador'.
          -> Jogador  -- ^ O estado do 'Jogador' apos se movimentar.
moveMorto t m j@(Jogador pista dist vel cola (Morto timeout)) | (timeout - t) > 0 = (Jogador pista dist vel cola (Morto t))
                                                              | otherwise = (Jogador pista dist vel cola (Chao False))

-- | Funcao que determina o movimento do jogador quando este se encontra no chao.
moveChao :: Double  -- ^ O tempo decorrido.
         -> Mapa    -- ^ O Mapa onde esta a ocorrer o jogo.
         -> Jogador -- ^ O estado anterior do 'Jogador'.
         -> Jogador -- ^ O estado do 'Jogador' apos se movimentar.
moveChao t m j@(Jogador pista dist vel cola (Chao estado)) | xf > proxDist && ficaChao m j && testaRampa j m == False = (Jogador pista proxDist vel cola (Chao estado))
                                                           | testaRampa j m == True && distRampa > proxDist = (Jogador pista proxDist vel cola (Chao estado))
                                                           | testaRampa j m == True = (Jogador pista (dist + distRampa) vel cola (Chao estado))
                                                           |  xf > proxDist && ficaChao m j == False && testaRampa j m == False = (Jogador pista proxDist vel cola (Ar h' inc' 0))
                                                           | otherwise = (Jogador pista (dist + (vel*t)) vel cola (Chao estado))
    where 
        x = fromIntegral(pista)
        proxDist = (fromIntegral(floor(dist)) + 1)
        h' = (obtemHjogador m j)
        inc' = (obtemIncJogador m j)
        distRampa = (vel*t) * (cos(grauRad(obtemInclinacaoPeca c)))
        c = encontraPecaMatriz (fromIntegral(pista), dist) m 
        xf = (dist + (vel)*t)

-- | Funcao que determina o movimento do jogador quando este se encontra no ar.
moveAr :: Double   -- ^ O tempo decorrido.
       -> Mapa     -- ^ O Mapa onde esta a ocorrer o jogo.
       -> Jogador  -- ^ O estado anterior do 'Jogador'.
       -> Jogador  -- ^ O estado do 'Jogador' apos se movimentar.
moveAr t m j@(Jogador pista dist vel cola (Ar h inc g)) | passaLimite t j && testeIntersecta t m j == False = (Jogador pista proxDist vel cola (Ar alt inc g))
                                                        | testeIntersecta t m j == False && passaLimite t j == False = (Jogador pista newDist vel cola (Ar h' inc g))
                                                        | testeIntersecta t m j && abs(diferenca) < 45 && nPassa = (Jogador pista novaDist vel cola (Chao False))
                                                        | testeIntersecta t m j && abs(diferenca) < 45 = (Jogador pista proxDist vel cola (Ar alt inc g))
                                                        | otherwise = (Jogador pista novaDist 0 cola (Morto 1))
        where 
            proxDist = ((fromIntegral(floor(dist))) + 1)
            alt = (posy(sePassaLimite t j))
            x = fromIntegral(pista) 
            newDist = (dist + (vel*cos(grauRad inc))*t)
            h' = (h + ((cordY(obtemCoordenadasV t j)) - g))
            diferenca = inc - (obtemInclinacaoPeca (encontraPecaMatriz (fromIntegral (pista), dist) m))
            novaDist = (posx(rectaOuPonto t ((encontraPecaMatriz (fromIntegral (pista), dist) m)) j))
            nPassa = novaDist < proxDist 

-- | Funcao que determina se o jogador esta no ar, morto, ou se esta no chao.
qualMove :: Double -> Mapa -> Jogador -> Jogador 
qualMove t m j@(Jogador pista dist vel cola (Chao estado)) = moveChao t m j
qualMove t m j@(Jogador pista dist vel cola (Ar h inc g)) = moveAr t m j 
qualMove t m j@(Jogador pista dist vel cola (Morto timeout)) = moveMorto t m j

-- | Verifica se o jogador passou do limite da pista.
passaLimite :: Double -> Jogador -> Bool 
passaLimite t j@(Jogador pista dist vel cola (Ar h inc g)) | caminhoPerc > ((fromIntegral(floor(dist))) + 1) = True 
                                                           | otherwise = False 
  where 
      caminhoPerc = (dist + ((vel*cos(grauRad (normalizaAngulo(inc)))*t)))

-- | Obtem as coordenadas do vetor da velocidade.                                                          
obtemCoordenadasV :: Double -> Jogador -> (Double, Double)
obtemCoordenadasV t j@(Jogador pista dist vel cola (Ar h inc g)) = (x, y)
  where 
      x = vel*cos (grauRad (normalizaAngulo (inc)))*t
      y = vel*sin(grauRad (normalizaAngulo(inc)))*t

-- | Obtem a coordenada x de um ponto ou vetor
cordX :: (Double, Double) -> Double 
cordX (x,y) = x

-- | Obtem a coordenada Y de um ponto ou vetor.
cordY :: (Double, Double) -> Double 
cordY (x,y) = y

-- | Verifica se a mota intersecta com o chao.
intersetaChao :: Double -> Peca -> Jogador -> Bool 
intersetaChao t (Recta _ h3) j@(Jogador pista dist vel cola (Ar h inc g)) | (h + ((cordY(obtemCoordenadasV t j)) - g)) < fromIntegral(h3) = True 
                                                                          | otherwise = False 
intersetaChao t (Rampa _ h1 h2) j@(Jogador pista dist vel _ (Ar h inc g)) = intersetam (Cartesiano x1 y1 , Cartesiano x2 y2) (Cartesiano x3 y3, Cartesiano x4 y4)
    where 
        x1 = fromIntegral (floor dist)
        y1 = fromIntegral h1  
        x2 = x1 + 1
        y2 = fromIntegral(h2) 
        x3 = dist  
        y3 = h 
        x4 = (dist + (vel*cos(grauRad inc))*t)
        y4 = (h + ((cordY(obtemCoordenadasV t j)) - g))

-- | Obtem o Ponto de intersecao da mota com o chao no caso de essa intersecao ser com uma 'Recta'.
obtemPontoRecta :: Double -> Peca -> Jogador -> Ponto
obtemPontoRecta t (Recta _ h3) j@(Jogador pista dist vel cola (Ar h inc g)) = intersecao (Cartesiano x1 y1, Cartesiano x2 y2) (Cartesiano x3 y3, Cartesiano x4 y4)
    where 
        x1 = fromIntegral (floor dist)
        y1 = fromIntegral h3 
        x2 = x1 + 1
        y2 = fromIntegral h3 
        x3 = dist  
        y3 = h 
        x4 = (dist + (vel*cos(grauRad inc))*t)
        y4 = (h + ((cordY(obtemCoordenadasV t j)) - g))

-- | Obtem a intersecao da mota com o chao no caso de essa intersecao ser com uma 'Rampa'.
obtemPontoRampa :: Double -> Peca -> Jogador -> Ponto 
obtemPontoRampa t (Rampa _ h1 h2) j@(Jogador pista dist vel cola (Ar h inc g)) = intersecao (Cartesiano c1 b1, Cartesiano c2 b2) (Cartesiano c3 b3, Cartesiano c4 b4)
    where 
        c1 = fromIntegral (floor dist)
        b1 = fromIntegral h1  
        c2 = c1 + 1
        b2 = fromIntegral(h2) 
        c3 = dist  
        b3 = h 
        c4 = (dist + (vel*cos(grauRad inc))*t)
        b4 = (h + ((cordY(obtemCoordenadasV t j)) - g))

-- | Decide se a funcao 'moveAr' vai usar o ponto de intersecao da mota com uma 'Rampa' ou com uma 'Recta'.
rectaOuPonto :: Double -> Peca -> Jogador -> Ponto 
rectaOuPonto t p@(Recta _ _) j@(Jogador pista dist vel cola (Ar h inc g)) = obtemPontoRecta t p j 
rectaOuPonto t p@(Rampa _ _ _) j@(Jogador pista dist vel cola (Ar h inc g)) = obtemPontoRampa t p j 

{-| No caso de a mota passar o limite da 'Peca', 
esta funcao irá determinar a altura da mota quando chega a este limite.-}
sePassaLimite :: Double -> Jogador -> Ponto 
sePassaLimite t j@(Jogador pista dist vel cola (Ar h inc g)) = intersecao (Cartesiano x1 y1, Cartesiano x2 y2) (Cartesiano x3 y3, Cartesiano x4 y4)
    where 
        x1 = ((fromIntegral(floor(dist))) + 1)
        y1 = 2 
        x2 = x1 
        y2 = -2 
        x3 = dist 
        y3 = h 
        x4 = (dist + (vel*cos(grauRad inc))*t)
        y4 = (h + ((cordY(obtemCoordenadasV t j)) - g))

-- | Testa se um jogador permanece no Chao ou passa a estar no Ar na funcao 'moveChao'.
ficaChao :: Mapa -> Jogador -> Bool 
ficaChao m j@(Jogador pista dist vel cola (Chao estado)) = (medeInclinacao (encontraPecaMatriz (x, y) m) (proximaPecaMatriz (x,y) m))
    where 
        x = fromIntegral(pista)
        y = dist

-- | Obtem a inclinacao do jogador na funcao 'moveChao' no caso de o jogador passar a estar no Ar.
obtemIncJogador :: Mapa -> Jogador -> Double 
obtemIncJogador m j@(Jogador pista dist vel cola (Chao estado)) = (obtemInclinacaoPeca (encontraPecaMatriz (fromIntegral(pista), dist) m))

-- | Obtem a altura de um jogador numa determinada posicao. 
obtemHjogador :: Mapa -> Jogador -> Double 
obtemHjogador m j@(Jogador pista dist vel cola (Chao estado)) = obtemAltura (encontraPecaMatriz (x, dist) m)
  where 
      x = fromIntegral(pista)

-- * Funcoes auxiliares 
-- | Obtem uma Peca num dado indice da lista.
encontraPecaLista :: Double -> Pista -> Peca 
encontraPecaLista n (x:xs) | n == 0 || length (x:xs) <= 1 = x
                           | otherwise = encontraPecaLista (n-1) xs 

-- | Aplica a funcao 'encontraPecaLista' num dado indice da matriz.
encontraPecaMatriz :: (Double,Double) -> Mapa -> Peca 
encontraPecaMatriz (n,m) (x:xs) | n == 0 || length (x:xs) <= 1 = encontraPecaLista (fromIntegral(floor(m))) x 
                                | otherwise = encontraPecaMatriz (n-1,fromIntegral(floor(m))) xs 

{- | Encontra o proximo elemento de uma dada posicao da lista 
(dado o indice 1, encontra o elemento da lista na posicao 2).-}
proximaPecaLista :: Double -> Pista -> Peca
proximaPecaLista n (x:xs) | n >= ((fromIntegral(length (x:xs))) -1) = x
                          | otherwise = encontraPecaLista (n+1) (x:xs)

-- | Aplica a funcao 'proximaPecaLista' em uma matriz.
proximaPecaMatriz :: (Double,Double) -> Mapa -> Peca 
proximaPecaMatriz (n,m) (x:xs) | n == 0 || length (x:xs) <= 1 = proximaPecaLista (fromIntegral(floor(m))) x 
                               | otherwise = proximaPecaMatriz ((n-1), fromIntegral(floor(m))) xs 

-- | Obtem a altura de uma uma dada peca.
obtemAltura :: Peca -> Double 
obtemAltura (Recta _ h) = fromIntegral h
obtemAltura (Rampa _ h1 h2) = fromIntegral h2 

-- | Obtem a inclinacao de uma dada peca.
obtemInclinacaoPeca :: Peca -> Double 
obtemInclinacaoPeca (Rampa _ h1 h2) = (atan (fromIntegral(h2))) * (180/pi)
obtemInclinacaoPeca (Recta _ _) = 0

{- | Compara as inclinacoes de duas pecas e determina se a mota vai ficar no chao ou no ar 
(Se retornar um True fica no chao e se retornar um False fica no ar).-}
medeInclinacao :: Peca -> Peca -> Bool 
medeInclinacao (Recta _ h) (Recta _ h1) | h == h1 = True 
medeInclinacao (Recta _ h) (Rampa _ h1 h2) | atan (fromIntegral(h2)) > 0 = True  
medeInclinacao (Rampa _ h1 h2) (Rampa _ h3 h4) | h2 < h4 && atan (fromIntegral(h2)) > atan (fromIntegral(h4)) = True  
                                               | h2 == h3 = True  
                                               | h2 > h4 = False                                    
medeInclinacao (Rampa _ h1 h2) (Recta _ h) | h1 < h2 = False

-- | Converte Graus em Radianos.
grauRad :: Double -> Double 
grauRad n = (n * (pi/180))

-- | Converte Radianos em Graus.
radGrau :: Double -> Double 
radGrau n = (n * (pi/180))

-- | Testa se uma Recta intersecta com a outra.
testeIntersecta :: Double -> Mapa -> Jogador -> Bool 
testeIntersecta t m j@(Jogador pista dist vel cola (Ar h inc g)) = (intersetaChao t (encontraPecaMatriz (x, dist) m) j)
  where x = fromIntegral(pista)

-- | Testa se a posicao em que o jogador se encontra é uma Rampa ou uma Recta.
testaRampa :: Jogador -> Mapa -> Bool 
testaRampa j@(Jogador pista dist vel cola (Chao estado)) m = estaEmRampa x 
  where   
      x = encontraPecaMatriz (fromIntegral(pista), dist) m 

-- | Funcao auxiliar para a funcao 'testaRampa' que retorna um False no caso de o input ser uma Recta e vice-versa.
estaEmRampa :: Peca -> Bool 
estaEmRampa (Recta _ _) = False 
estaEmRampa (Rampa _ _ _) = True 

-- | Verifica se o jogador esta ou nao no chao.
arChao :: Double -> Mapa -> Jogador -> Jogador 
arChao t m j@(Jogador pista dist vel cola (Chao estado)) = atualizaVelocidade t m j 
arChao t m j@(Jogador pista dist vel cola (Ar h inc g)) = atualizaVelocidadeAr t m j


