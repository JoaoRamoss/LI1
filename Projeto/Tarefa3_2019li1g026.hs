-- | Este módulo define funções comuns da Tarefa 3 do trabalho prático.
module Tarefa3_2019li1g026 where

    import LI11920
    
    import Data.List
    -- * Testes
    
    -- | Testes unitários da Tarefa 3.
    --
    -- Cada teste é um 'Mapa'.
    testesT3 :: [Mapa]
    testesT3 = [
                [[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Terra 0]],
                [[Recta Terra 0, Rampa Lama 0 2, Rampa Lama 2 0], [Recta Terra 0, Recta Relva 0, Rampa Lama 0 2]], 
                [[Recta Terra 0, Recta Terra 0, Rampa Relva 0 2, Rampa Relva 2 4, Rampa Relva 4 0], [Recta Terra 0, Recta Lama 0, Rampa Relva 0 2, Recta Terra 2, Recta Boost 2]],
                [[Recta Terra 0, Recta Terra 0, Recta Relva 0], [Recta Terra 0, Recta Terra 0, Rampa Boost 0 2]],
                [[Recta Terra 0, Recta Boost 0, Recta Boost 0, Rampa Lama 0 4, Rampa Relva 4 2, Recta Boost 2, Rampa Relva 2 0, Recta Terra 0], [Recta Terra 0, Recta Boost 0, Recta Boost 0, Rampa Lama 0 1, Rampa Relva 1 2, Recta Boost 2, Rampa Lama 2 0, Recta Relva 0]],
                [[Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0],[Recta Terra 0,Recta Lama 0,Recta Terra 0,Recta Terra 0],[Recta Terra 0,Rampa Lama 0 2,Rampa Lama 2 0,Rampa Relva 0 1]],
                [[Recta Terra 0,Recta Boost 0,Recta Boost 0],[Recta Terra 0,Recta Terra 0,Recta Lama 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0],[Recta Terra 0,Rampa Lama 0 2,Rampa Lama 2 0],[Recta Terra 0,Rampa Relva 0 1,Rampa Relva 1 0]],
                [[Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Terra 0,Rampa Terra 0 1,Recta Terra 1,Rampa Terra 1 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Lama 0,Recta Relva 0,Recta Terra 0]],
                [[Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Terra 0,Rampa Terra 0 1,Recta Terra 1],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Lama 0],[Recta Terra 0,Recta Relva 0,Recta Terra 0,Recta Relva 0,Rampa Boost 0 2,Rampa Terra 2 4],[Recta Terra 0,Recta Lama 0,Recta Relva 0,Recta Relva 0,Recta Relva 0,Recta Boost 0],[Recta Terra 0,Rampa Relva 0 2,Recta Relva 2,Recta Terra 2,Recta Terra 2,Recta Boost 2],[Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0,Recta Lama 0,Recta Relva 0]],
                [[Recta Terra 0,Recta Terra 0],[Recta Terra 0,Recta Terra 0],[Recta Terra 0,Recta Relva 0],[Recta Terra 0,Recta Terra 0],[Recta Terra 0,Recta Boost 0]],
                [[Recta Terra 0,Recta Terra 0],[Recta Terra 0,Recta Terra 0]],
                [[Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0],[Recta Terra 0,Recta Lama 0,Recta Terra 0,Recta Terra 0],[Recta Terra 0,Rampa Lama 0 2,Rampa Lama 2 0,Rampa Relva 0 1],[Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Terra 0],[Recta Terra 0,Rampa Terra 0 2,Rampa Boost 2 0,Recta Terra 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Lama 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Relva 0]],
                [[Recta Terra 0,Recta Terra 0],[Recta Terra 0,Recta Relva 0],[Recta Terra 0,Recta Terra 0],[Recta Terra 0,Recta Relva 0],[Recta Terra 0,Rampa Terra 0 2]],
                [[Recta Terra 0,Recta Terra 0],[Recta Terra 0,Recta Terra 0],[Recta Terra 0,Recta Relva 0],[Recta Terra 0,Recta Terra 0]],
                [[Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0,Recta Lama 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Lama 0 2,Rampa Lama 2 0],[Recta Terra 0,Rampa Relva 0 1,Rampa Relva 1 0,Recta Lama 0,Recta Terra 0]],
                [[Recta Terra 0,Recta Terra 0,Recta Terra 0],[Recta Terra 0,Recta Relva 0,Recta Terra 0],[Recta Terra 0,Recta Boost 0,Recta Boost 0],[Recta Terra 0,Recta Relva 0,Recta Relva 0],[Recta Terra 0,Recta Relva 0,Recta Relva 0],[Recta Terra 0,Recta Relva 0,Recta Terra 0]],
                [[Recta Terra 0,Recta Terra 0,Recta Relva 0],[Recta Terra 0,Recta Terra 0,Recta Relva 0],[Recta Terra 0,Rampa Terra 0 2,Rampa Terra 2 0]],
                [[Recta Terra 0,Recta Relva 0,Recta Relva 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0],[Recta Terra 0,Rampa Terra 0 2,Rampa Terra 2 3],[Recta Terra 0,Recta Terra 0,Recta Lama 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0],[Recta Terra 0,Recta Boost 0,Recta Lama 0]]
               ]
    -- * Funções principais da Tarefa 3.
    
    -- | Desconstrói um 'Mapa' numa sequência de 'Instrucoes'.
    --
    -- __NB:__ Uma solução correcta deve retornar uma sequência de 'Instrucoes' tal que, para qualquer mapa válido 'm', executar as instruções '(desconstroi m)' produza o mesmo mapa 'm'.
    --
    -- __NB:__ Uma boa solução deve representar o 'Mapa' dado no mínimo número de 'Instrucoes', de acordo com a função 'tamanhoInstrucoes'.
    desconstroi :: Mapa -> Instrucoes
    desconstroi (x:xs) = (converteMatriz (separaLista (juntaFinal (traduzListaVertical(traduzListaVertical (traduzMatriz 0 (tiraPrimeiro (x:xs))))))))

    -- * Funcoes auxiliares 
    
    -- | Tira o primeiro elemento de cada uma das listas de listas.
    tiraPrimeiro :: Mapa -> Mapa 
    tiraPrimeiro [] = []
    tiraPrimeiro (x:xs) = drop 1 x : tiraPrimeiro xs
    
    -- | Traduz uma peca em uma instrucao para os Bulldozers. 
    traducao :: Int -> Peca -> Instrucao 
    traducao n (Recta tipo h) = Anda [n] tipo 
    traducao n (Rampa tipo h1 h2) | h1 > h2 = Desce [n] tipo (h1-h2)
                                  | h1 < h2 = Sobe [n] tipo (h2-h1)
    
    -- | Traduz uma lista de Pecas em uma lista de Instrucoes
    traduzLista :: Int -> [Peca] -> [Instrucao]
    traduzLista n [] = []
    traduzLista n (x:xs) = traducao n x : traduzLista n xs 
    
    -- | Traduz uma lista de listas de Pecas em uma matriz de instrucoes.
    traduzMatriz :: Int -> [[Peca]] -> [[Instrucao]]
    traduzMatriz _ [] = []
    traduzMatriz n (x:xs) | m == 0 = []
                          | otherwise = traduzLista n x : traduzMatriz (n+1) xs 
        where
            m = length (x:xs)                
    
    
    -- * Padrao vertical
    -- | Verifica se duas instrucoes sao Iguais.
    saoIguais :: Instrucao -> Instrucao -> Bool 
    saoIguais (Anda n piso) (Anda m piso1) | piso == piso1 = True
                                               | otherwise = False 
    
    saoIguais (Desce n piso n1) (Desce m piso1 m1) | piso == piso1 && m1 == n1 = True   
                                                       | otherwise = False 
    
    saoIguais (Sobe n piso n1 ) (Sobe m piso1 m1) | piso1 == piso && m1 == n1 = True 
                                                      | otherwise = False 
    saoIguais _ _ = False 
    
    -- | No caso de duas pecas serem iguais, devolve uma lista com padroes verticais.
    traduzPeca :: Instrucao -> Instrucao -> [Instrucao]
    traduzPeca (Anda n piso) (Anda m piso1) | piso1 == piso = [Anda (n++m)piso1]
    traduzPeca (Desce n piso n1) (Desce m piso1 m1) | piso1 == piso && n1 == m1 = [Desce (n++m)piso1 m1]
    traduzPeca (Sobe n piso n1 ) (Sobe m piso1 m1) | piso1 == piso && n1 == m1 = [Sobe (n++m)piso1 m1]
    
    -- | Aplica a funcao 'traduzPeca' em duas listas.
    comparaListas :: [Instrucao] -> [Instrucao] -> [Instrucao]
    comparaListas l [] = l
    comparaListas [x] (y:ys) | saoIguais x y = traduzPeca x y ++ ys 
                             | otherwise = [y] ++ comparaListas [x] ys 
                             | otherwise = [x] ++ comparaListas [x] ys 
    comparaListas (x:xs) (y:ys) | saoIguais x y = traduzPeca x y ++ comparaListas xs ys 
                                | otherwise = [x] ++ [y] ++ comparaListas xs ys
    
    -- | Aplica a funcao 'comparaListas' em uma matriz.
    traduzListaVertical :: [[Instrucao]] -> [[Instrucao]]
    traduzListaVertical [] = []
    traduzListaVertical [x] = [x]
    traduzListaVertical (x:y:xs) = comparaListas x y : traduzListaVertical xs 

    -- | Transforma a matriz gerada em 'traduzListaVertical' em uma lista, que poderá ser usada depois na funcao 'separaLista'.
    juntaFinal :: [[Instrucao]] -> [Instrucao]
    juntaFinal [] = []
    juntaFinal (x:xs) = x ++ juntaFinal xs 
    
    -- * Padrao Horizontal
    -- | Usa a funcao predefinida 'group' para juntar as instrucoes iguais.
    separaLista :: [Instrucao] -> [[Instrucao]]
    separaLista [] = []
    separaLista l = group l 
    
    -- | Numa lista de listas de instrucoes, transforma as listas cujos elementos sao iguais em uma instrucao "Repete" 
    converteLista :: [Instrucao] -> Instrucao
    converteLista [x] = x 
    converteLista (x:xs) = Repete (length (x:xs)) [x] 
                         
    -- | Aplica a funcao 'converteLista' em uma lista de listas.
    converteMatriz :: [[Instrucao]] -> [Instrucao] 
    converteMatriz [] = []
    converteMatriz (x:xs) = [converteLista x] ++ converteMatriz xs     