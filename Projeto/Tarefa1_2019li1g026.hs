-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa1_2019li1g026 where

    import LI11920
    import System.Random
    import Data.List
    
    -- * Testes
    
    -- | Testes unitários da Tarefa 1.
    --
    -- Cada teste é um triplo (/número de 'Pista's/,/comprimento de cada 'Pista' do 'Mapa'/,/semente de aleatoriedades/).
    testesT1 :: [(Int,Int,Int)]
    testesT1 = [(0,5,7), (3,3,3), (9,5,3), (1,6,4), (4,7,4), (9,3,7), (6,6,6), (7,3,1), (2,7,3), (3,1,9), (1,9,6), (5,1,7), (1,1,7), (5,1,8),(2,4,6),(3,5,1),(4,4,2),(5,3,1),(1,3,1),(4,6,9),(2,2,2),(1,5,3),(7,7,4),(4,5,2),(8,9,7),(4,2,9),(3,9,2),(5,9,8)]
    
    -- * Funções pré-definidas da Tarefa 1.
    
    geraAleatorios :: Int -> Int -> [Int]
    geraAleatorios n seed = take n (randomRs (0,9) (mkStdGen seed))
    
    -- * Funções principais da Tarefa 1.
    
    gera :: Int -> Int -> Int -> Mapa
    gera npistas comprimento semente | npistas == 0 = []
                                     | comprimento == 1 = geraRetaTerra 1 : gera (npistas - 1) comprimento semente 
                                     | otherwise = resultadoFinal npistas comprimento semente

    -- * Funcoes auxiliares.
    -- | Gera o primeiro elemento de cada pista.
    geraRetaTerra :: Int -> [Peca]
    geraRetaTerra n = replicate n (Recta Terra 0)

    -- | Gera uma lista de numeros aleatorios usando a funcao 'geraAleatorios'.
    geraListaAleat :: Int -> Int -> Int -> [Int]
    geraListaAleat n c seed = geraAleatorios (((c-1)*n)*2) seed

    -- | Transforma uma lista em uma lista de tuplas.
    transformaListaEmTupla :: [Int] -> [(Int,Int)]
    transformaListaEmTupla [] = []
    transformaListaEmTupla (x:y:xs) | (mod (length (x:y:xs)) 2 == 0) = (x,y):transformaListaEmTupla xs

    -- | Divide uma lista em em n grupos de listas.
    divideEmGrupos :: Int -> [(Int,Int)] ->[[(Int,Int)]] 
    divideEmGrupos 0 _ = []
    divideEmGrupos _ [] = []
    divideEmGrupos n ls = take n ls : divideEmGrupos n (drop n ls)

    -- | Converte o primeiro valor de uma Tupla em Piso
    gamaPiso :: Peca -> (Int, Int) -> Piso
    gamaPiso (Recta tipo _) (x,y) | x >= 0 && x <= 1 = Terra 
                                  | x >= 2 && x <= 3 = Relva
                                  | x == 4 = Lama 
                                  | x == 5 = Boost
                                  | x >= 6 && x <= 9 = tipo 
                                  
    gamaPiso (Rampa tipo _ _) (x,y) | x >= 0 && x <= 1 = Terra 
                                    | x >= 2 && x <= 3 = Relva
                                    | x == 4 = Lama 
                                    | x == 5 = Boost
                                    | x >= 6 && x <= 9 = tipo 

    -- | Converte uma tupla em uma Peca, usando a funcao 'gamaPiso' para determinar o piso, e o segundo elemento da tupla para determinar o tipo.
    gamaTipo :: Peca -> (Int,Int) -> Peca
    gamaTipo (Rampa tipo h1 h2) (x,y) | y >= 0 && y <= 1 = (Rampa (gamaPiso (Rampa tipo h1 h2) (x,y)) h2 (h2+(y+1)))
                                      | y >= 2 && y <= 5 && h2 == 0 = (Recta (gamaPiso (Rampa tipo h1 h2) (x,y)) h2)
                                      | y >= 2 && y <= 5 && h2-(y-1) > 0 = (Rampa (gamaPiso (Rampa tipo h1 h2) (x,y)) h2 (h2-(y-1)))
                                      | y >= 2 && y <= 5 && h2 - (y-1) <= 0 = (Rampa (gamaPiso (Rampa tipo h1 h2) (x,y)) h2 0)
                                      | y >= 6 && y <= 9 = (Recta (gamaPiso (Rampa tipo h1 h2) (x,y)) h2)
                                      | otherwise = (Recta (gamaPiso (Rampa tipo h1 h2) (x,y)) h2)  
                                      

    gamaTipo (Recta tipo h) (x,y) | y >= 0 && y <= 1 = (Rampa (gamaPiso (Recta tipo h) (x,y)) h (h+(y+1)))
                                  | y >= 2 && y <= 5 && h == 0 = (Recta (gamaPiso (Recta tipo h) (x,y)) h)
                                  | y >= 2 && y <= 5 && h-(y-1) > 0 = (Rampa (gamaPiso (Recta tipo h) (x,y)) h (h-(y-1)))
                                  | y >= 2 && y <= 5 && h - (y-1) <= 0 = (Rampa (gamaPiso (Recta tipo h) (x,y)) h 0)
                                  | y >= 6 && y <= 9 = (Recta (gamaPiso (Recta tipo h) (x,y)) h)
                                  | otherwise = (Recta (gamaPiso (Recta tipo h) (x,y)) h)
    

 
    -- * Funcoes finais para obter um Mapa.
    -- | Gera uma lista de listas de tuplas usando as funcoes 'divideEmGrupos', 'transformaListaEmTupla' e 'geraListaAleat'              
    tuplasAleatorias :: Int -> Int -> Int -> [[(Int,Int)]]
    tuplasAleatorias n c seed = (divideEmGrupos (c-1) (transformaListaEmTupla (geraListaAleat n c seed)))
    
    -- | Converte uma lista de tuplas em uma Pista usando a funcao 'gamaTipo'.
    atualizaTupla :: Peca -> [(Int,Int)] -> Pista
    atualizaTupla _ [] = []
    atualizaTupla p ((x,y):xs) = ((gamaTipo p (x,y)) : atualizaTupla (gamaTipo p (x,y)) xs)
    
    -- | Converte uma lista de listas de tuplas em um Mapa.
    atualizaMapa :: [[(Int,Int)]] -> Mapa 
    atualizaMapa [] = []
    atualizaMapa (x:xs) = atualizaTupla (Recta Terra 0) x : atualizaMapa xs 

    -- | Funcao para obter um Mapa (sem o devido primeiro elemento, Recta Terra 0).
    atualizaResto :: Int -> Int -> Int -> Mapa 
    atualizaResto n c seed = atualizaMapa (tuplasAleatorias n c seed)

    -- | Funcao para adicionar Recta Terra 0 como primeiro elemento de cada Pista. 
    juntaListas :: Mapa -> Mapa 
    juntaListas [] = []
    juntaListas (y:ys) = ([Recta Terra 0] ++ y) : juntaListas ys

    -- | Ultima funcao da tarefa, tem como objetivo juntar as funcoes 'atualizaResto' e 'juntaListas' para finalmente gerar o Mapa que foi pedido pelo utilizador.
    resultadoFinal :: Int -> Int -> Int -> Mapa 
    resultadoFinal n c seed = juntaListas (atualizaResto n c seed)
        