-- | Este módulo define funções comuns da Tarefa 6 do trabalho prático.
module Tarefa6_2019li1g026 where

    import LI11920
    import Tarefa0_2019li1g026
    import Tarefa2_2019li1g026
    import Tarefa4_2019li1g026
    import Data.List
    
-- * Introdução do Relatório
--
-- $introduçãoRelatório
--
--Na última Tarefa deste projeto, o objetivo principal é criar um bot que tome as suas próprias decisões 
--perante o estado atual do jogo. O propósito deste robot vai ser para o caso de um jogador querer jogar em modo 
--de um jogador. O seu principal foco é ser o mais rápido possível, tentando vencer a corrida. 
--Posto isto, o nosso objetivo é criar um bot que seja o mais proativo possível, para que possa
--sair vitorioso nos confrontos com outras "yike bikes" e para que cause o caos na pista de corrida.

-- * Objetivos da Tarefa
--
-- $objetivosTarefa
--
--Na avaliação do robot, elementos como a audácia e a inteligência são os fatores principais. 
--Com isto em mente, a estratégia deliniada foi definir uma função para que o robot analise o estado em que se encontra 
--o jogo. Perante este estado o robot vai deliberar se avança para uma posiçao melhor, mudando de pista ou avançando. 
--Esta decisão vai ser tomada em função da posição do jogador pelo qual ele estará a executar a jogada, o mapa  
--e o tipo do piso das peças circundantes com o único propósito de vencer a corrida, ou seja, ser mais rápido possível.

    -- * Funções principais da Tarefa 6.
    
    -- | Define um ro'bot' capaz de jogar autonomamente o jogo.
    bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
        -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
        -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
    bot id e = analisaSituacao id e
    
    {- | Com base nas informacoes que recebe, determina qual a melhor jogada que esta disponivel para o bot. -}
    analisaSituacao :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
                    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
                    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
    analisaSituacao id e@(Estado m l) | estaChaoT j == False && estaArT j == False = Just Acelera
                                      | estaChaoT j == False && estaArT j = anguloIdeal m j
                                      | estaChaoT j = baixoOuCima m j
                                      | otherwise = Nothing
        where
            j = encontraJogador id l
    
    {- | Tendo em conta as funcoes 'trocarCima' e 'trocarBaixo' determina qual dessas duas sera aquela que sera a melhor
    opcao para o bot. -}
    baixoOuCima :: Mapa         -- ^ O Mapa em que esta a ocorrer o jogo.
                -> Jogador      -- ^ O Jogador identificado pelo Int na funcao 'bot'.
                -> Maybe Jogada -- ^ A Jogada a efetuar pelo 'bot'.
    baixoOuCima m j | trocarBaixo m j < trocarCima m j && trocarBaixo m j < somaAtritos j m && condicoesBaixo m j = Just (Movimenta B)
                    | trocarCima m j < trocarBaixo m j && trocarCima m j < somaAtritos j m && condicoesCima m j = Just (Movimenta C)
                    | trocarCima m j == trocarBaixo m j && trocarCima m j < somaAtritos j m && condicoesCima m j = Just (Movimenta C)
                    | trocarCima m j == trocarBaixo m j && trocarCima m j < somaAtritos j m && condicoesBaixo m j = Just (Movimenta B)
                    | otherwise = Nothing
    
    -- | Quando o jogador esta no ar, faz com que o jogador altere o seu angulo de modo a nao morrer quando em contacto com o chao.
    anguloIdeal :: Mapa         -- ^ O Mapa em que esta a ocorrer o jogo.
                -> Jogador      -- ^ O Jogador identificado pelo Int na funcao 'bot'.
                -> Maybe Jogada -- ^ A Jogada a efetuar pelo 'bot'.
    anguloIdeal m j@(Jogador pista dist vel cola (Ar h inc g)) | morrePos = Just (Movimenta D)
                                                               | morreNeg = Just (Movimenta E)
                                                               | anguloFrente m j && inc > 0 = Just (Movimenta D)
                                                               | anguloFrente m j && inc < 0 = Just (Movimenta E)
                                                               | otherwise = Nothing
        where
            diferenca = inc - (obtemInclinacaoPeca (encontraPecaMatriz (fromIntegral (pista), dist) m))
            morrePos = abs(diferenca) >= 45 && inc > 0
            morreNeg = abs(diferenca) >= 45 && inc < 0
    
    
    -- * Funcoes Auxiliares
    
    -- | Compara o atrito da peca para onde quer saltar com o atrito da peca onde esta e devolve True se compensar saltar para a outra peca.
    comparaAtrito :: Peca -> Peca -> Bool
    comparaAtrito p1 p2 | obtemAtrito p1 < obtemAtrito p2 = True
                        | otherwise = False
    
    -- | Verifica se o jogador esta no chao.
    estaChaoT :: Jogador -> Bool
    estaChaoT (Jogador pista dist vel cola (Chao True)) = True
    estaChaoT _ = False
    
    -- | Verifica se o jogador esta no ar.
    estaArT :: Jogador -> Bool
    estaArT (Jogador pista dist vel cola (Ar h inc g)) = True
    estaArT _ = False
    
    -- | Verifica se compensa ao Bot saltar para a pista de cima.
    trocarCima :: Mapa -> Jogador -> Double
    trocarCima m j@(Jogador pista dist vel cola estado) | pista == 0 = 10000
                                                        | otherwise = somaAtritos k m
        where
            k = (Jogador (pista - 1) dist vel cola estado)
    
    -- | Verifica se compensa ao Bot saltar para a pista de baixo.
    trocarBaixo :: Mapa -> Jogador -> Double
    trocarBaixo m j@(Jogador pista dist vel cola estado) | pista >= length m = 10000
                                                         | otherwise = somaAtritos k m
        where
            k = (Jogador (pista + 1) dist vel cola estado)
    
    -- | Faz com que a funcao 'bot' apenas actue no jogador que foi identificado no primeiro argumento desta.
    encontraJogador :: Int -> [Jogador] -> Jogador
    encontraJogador n (x:xs) | n == 0 = x
                             | otherwise = encontraJogador (n-1) xs
    
    
    -- | Verifica se é possivel para o bot saltar para a pista de cima (se nao houver uma pista de cima, entao ele nao tem as condicoes).
    condicoesCima :: Mapa -> Jogador -> Bool
    condicoesCima m j@(Jogador pista dist _ _ _) | pista /= 0 && podeTransitarFx p1 p2= True
                                                 | otherwise = False
        where
            p1 = encontraPecaMatriz (fromIntegral pista, dist) m
            p2 = encontraPecaMatriz (fromIntegral (pista - 1),dist) m
    
    
    
    -- | Verifica se é possivel para o bot saltar para a pista de baixo (Se ele estiver na ultuma pista, nao tem condicoes para o fazer).
    condicoesBaixo :: Mapa -> Jogador -> Bool
    condicoesBaixo m j@(Jogador pista dist _ _ _) | pista < length m && podeTransitarFx p1 p2= True
                                                  | otherwise = False
        where
            p1 = encontraPecaMatriz (fromIntegral pista, dist) m
            p2 = encontraPecaMatriz (fromIntegral (pista + 1), dist) m
    
    
    -- | Soma o valor dos atritos de tres pecas consecutivas e retorna o valor dessa soma.
    somaAtritos :: Jogador -> Mapa -> Double
    somaAtritos j@(Jogador pista dist _ _ (Chao _)) m | dist <= limiteabs = r1 + r2 + r3
                                                      | dist <= limite2 = r2 + r1
                                                      | otherwise = r2
        where
            r1 = obtemAtrito (encontraPecaMatriz (fromIntegral (pista), (fromIntegral(floor(dist)) + 1)) m)
            r2 = obtemAtrito (encontraPecaMatriz (fromIntegral (pista), dist) m)
            r3 = obtemAtrito (encontraPecaMatriz (fromIntegral (pista), (fromIntegral(floor(dist)) + 2)) m)
            limite2 = fromIntegral (length(head m) - 2)
            limiteabs = fromIntegral (length(head m) - 3)
    
    -- | Analiza se o jogador no ar vai morrer na peca que vai encontrar a frente.
    anguloFrente :: Mapa -> Jogador -> Bool
    anguloFrente m j@(Jogador pista dist vel cola (Ar h inc g)) | abs(diferenca) >= 45 = True
                                                                | otherwise = False
        where
            diferenca = inc - (obtemInclinacaoPeca (encontraPecaMatriz (fromIntegral (pista), (fromIntegral(floor(dist)) + 1))m))

    -- | Função que verifica se as alturas entre as duas pistas são válidas para o jogador trocar de pista Peca -> Peca -> Bool
    podeTransitarFx :: Peca -> Peca -> Bool
    podeTransitarFx (Recta tipo h) (Recta tipo1 h1)  =  if ((difAltura (Recta tipo h) (Recta tipo1 h1))<=0.2) || h > h1 then True else False
    podeTransitarFx (Recta tipo h) (Rampa tipo1 h1 h2) =  if ((difAltura (Recta tipo h) (Rampa tipo1 h1 h2))<=0.2) then True else False
    podeTransitarFx (Rampa tipo h1 h2) (Rampa tipo1 h3 h4) = if ((difAltura (Rampa tipo h1 h2) (Rampa tipo1 h3 h4)) <=0.2) then True else False
    podeTransitarFx (Rampa tipo h1 h2) (Recta tipo1 h) =   if ((difAltura (Rampa tipo h1 h2) (Recta tipo1 h))<=0.2) then True else False
    
    
-- $ Conclusão do Relatório
--
-- *conclusãoRelatório
--
--Depois da conclusão da função principal da tarefa 6, obtemos um bot capaz de tomar decisões perante o estado atual, 
--tendo a sua posição como fator principal. O bot faz o corredor mover-se na direção da peça com menor atrito e 
--se é vantajoso mudar de pista.
--Desta forma, o bot está pronto para ser utilizado no modo solo no jogo final, na interface produzida na tarefa anterior.
    
    