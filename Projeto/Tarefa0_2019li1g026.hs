-- | Este módulo define funções genéricas sobre vetores e matrizes, que serão úteis na resolução do trabalho prático.
module Tarefa0_2019li1g026 where


-- * Funções não-recursivas.

-- | Um ponto a duas dimensões dado num referencial cartesiado (distâncias aos eixos vertical e horizontal)
--
-- <<http://li1.lsd.di.uminho.pt/images/cartesiano.png cartesisano>>
-- , ou num referencial polar (distância à origem e ângulo do respectivo vector com o eixo horizontal).
--
-- <<http://li1.lsd.di.uminho.pt/images/polar.png polar>> 
data Ponto = Cartesiano Double Double | Polar Double Angulo 
             deriving (Show,Eq)

-- | Um ângulo em graus.
type Angulo = Double

-- ** Funções sobre vetores

-- | Um 'Vetor' na representação escalar é um 'Ponto' em relação à origem.
type Vetor = Ponto
-- ^ <<http://li1.lsd.di.uminho.pt/images/vetor.png vetor>>

-- *** Funções gerais sobre 'Vetor'es.

-- *** Funções gerais sobre 'Vetor'es.
rad2deg :: Angulo -> Angulo
rad2deg a = a * (pi / 180)

posx :: Vetor -> Double
posx (Cartesiano x y) = x
posx (Polar r a) = r * cos (rad2deg a) 

posy :: Vetor -> Double
posy (Cartesiano x y) = y
posy (Polar r a) = r * sin (rad2deg a) 

-- Soma dois Vetores
somaVetores :: Vetor -> Vetor -> Vetor
somaVetores v1 v2 = Cartesiano (posx v1 + posx v2) (posy v1 + posy v2)

-- | Subtrai dois Vetores
subtraiVetores :: Vetor -> Vetor -> Vetor
subtraiVetores v1 v2 = Cartesiano (posx v1 - posx v2) (posy v1 - posy v2)

-- | Multiplica um escalar por um 'Vetor'.
multiplicaVetor :: Double -> Vetor -> Vetor
multiplicaVetor x v1 = Cartesiano (x * posx v1) (x * posy v1)

-- ** Funções sobre rectas.

-- | Um segmento de reta é definido por dois pontos.
type Reta = (Ponto,Ponto)

-- | Testar se dois segmentos de reta se intersetam.
--
-- __NB:__ Aplique as equações matemáticas bem conhecidas, como explicado por exemplo em <http://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.
intersetam :: Reta -> Reta -> Bool
intersetam (p1,p2) (p3,p4) = let ta1 = (posy p3 - posy p4) * (posx p1 - posx p3) + (posx p4 - posx p3) * (posy p1 - posy p3)
                                 tb1 = (posy p1 - posy p2) * (posx p1 - posx p3) + (posx p2 - posx p1) * (posy p1 - posy p3) 
                                 tc = (posx p4 - posx p3) * (posy p1 - posy p2) - (posx p1 - posx p2) * (posy p4 - posy p3)
                                 ta = ta1 / tc 
                                 tb = tb1 / tc
                             in (ta >= 0) && (ta <= 1) && (tb >= 0) && (tb <= 1)

-- intersetam (Cartesiano x1 y1 , Cartesiano x2 y2) (Cartesiano x3 y3 , Cartesiano x4 y4) = (y2-y1)/(x2-x1) /= (y4-y3)/(x4-x3)
                                                                                        
                                           
-- | Calcular o ponto de intersecao entre dois segmentos de reta.
--
-- __NB:__ Aplique as equações matemáticas bem conhecidas, como explicado por exemplo em <http://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.
intersecao :: Reta -> Reta -> Ponto
intersecao (p1,p2) (p3,p4) = let ta1 = (posy p3 - posy p4) * (posx p1 - posx p3) + (posx p4 - posx p3) * (posy p1 - posy p3) 
                                 tc = (posx p4 - posx p3) * (posy p1 - posy p2) - (posx p1 - posx p2) * (posy p4 - posy p3)
                                 ta = ta1 / tc 
                             in Cartesiano (posx p1 + ta * (posx p2 - posx p1)) (posy p1 + ta * (posy p2 - posy p1))





-- intersecao (Cartesiano x1 y1 , Cartesiano x2 y2 ) (Cartesiano x3 y3 , Cartesiano x4 y4 ) =
--             Cartesiano (x1 + (((x3-x1)*(y4-y3) + (y1-y3)*(x4-x3)) / ((y4-y3)*(x2-x1) - (y2-y1)*(x4-x3))) * (x2-x1))
--                ( y1  + (((x3-x1)*(y4-y3) + (y1-y3)*(x4-x3)) / ((y4-y3)*(x2-x1) - (y2-y1)*(x4-x3))) * (y2-y1))
-- intersecao (p1,p2) (p3,p4) = intersecao (polar2cart p1, polar2cart p2) (polar2cart p3, polar2cart p4)

-- ** Funções sobre listas

-- *** Funções gerais sobre listas.
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | Verifica se o indice pertence à lista.
--
-- __Sugestão:__ use a função 'length' que calcula tamanhos de listas
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido n [] = False
eIndiceListaValido n l = n >= 0 && n < length l

-- ** Funções sobre matrizes.

-- *** Funções gerais sobre matrizes.

-- | A dimensão de um mapa dada como um par (/número de linhas/,/número de colunhas/).
type DimensaoMatriz = (Int,Int)

-- | Uma posição numa matriz dada como um par (/linha/,/colunha/).
-- As coordenadas são dois números naturais e começam com (0,0) no canto superior esquerdo, com as linhas incrementando para baixo e as colunas incrementando para a direita:
--
-- <<http://li1.lsd.di.uminho.pt/images/posicaomatriz.png posicaomatriz>>
type PosicaoMatriz = (Int,Int)

-- | Uma matriz é um conjunto de elementos a duas dimensões.
--
-- Em notação matemática, é geralmente representada por:
--
-- <<https://upload.wikimedia.org/wikipedia/commons/d/d8/Matriz_organizacao.png matriz>>
type Matriz a = [[a]]

-- | Calcula a dimensão de uma matriz.
--
-- __NB:__ Note que não existem matrizes de dimensão /m * 0/ ou /0 * n/, e que qualquer matriz vazia deve ter dimensão /0 * 0/.
--
-- __Sugestão:__ relembre a função 'length', referida anteriormente.
dimensaoMatriz :: Matriz a -> DimensaoMatriz   
dimensaoMatriz [] = (0,0)
dimensaoMatriz m = (lengthAux m , length (head m))
                 where lengthAux [] = 0
                       lengthAux (h:t) = if length h > 0
                                          then 1 + lengthAux t          
                                         else 0

-- | Verifica se a posição pertence à matriz.
ePosicaoMatrizValida :: PosicaoMatriz -> Matriz a -> Bool 
ePosicaoMatrizValida (r,c) m = let dim = dimensaoMatriz m
                               in (r >= 0 && r < fst dim) && (c >= 0 && c < snd dim)

-- * Funções recursivas.

-- ** Funções sobre ângulos

-- | Normaliza um ângulo na gama [0..360).
--  Um ângulo pode ser usado para representar a rotação
--  que um objecto efectua. Normalizar um ângulo na gama [0..360)
--  consiste, intuitivamente, em extrair a orientação do
    --  objecto que resulta da aplicação de uma rotação. Por exemplo, é verdade que:
--
-- prop> normalizaAngulo 360 = 0
-- prop> normalizaAngulo 390 = 30
-- prop> normalizaAngulo 720 = 0
-- prop> normalizaAngulo (-30) = 330
normalizaAngulo :: Angulo -> Angulo
normalizaAngulo a = if (a < 0) 
                       then normalizaAngulo (a + 360)
                    else if (a >= 360)
                       then normalizaAngulo (a - 360)
                    else a
   

-- ** Funções sobre listas.

-- | Devolve o elemento num dado índice de uma lista.
--
-- __Sugestão:__ Não use a função (!!) :: [a] -> Int -> a :-)
encontraIndiceLista :: Int -> [a] -> a
encontraIndiceLista p [] = error "empty"
encontraIndiceLista p (h:t) = if p == 0
                                  then h
                              else encontraIndiceLista (p-1) (t)      

-- | Modifica um elemento num dado índice.
--
-- __NB:__ Devolve a própria lista se o elemento não existir.
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista p x [] = []
atualizaIndiceLista p x (h:t) = if p == 0  
                                    then (x:t)
                                else (h : atualizaIndiceLista (p - 1) x t)    


-- ** Funções sobre matrizes.

-- | Devolve o elemento numa dada 'Posicao' de uma 'Matriz'.
encontraPosicaoMatriz :: PosicaoMatriz -> Matriz a -> a
encontraPosicaoMatriz (r,c) m = encontraIndiceLista c (encontraIndiceLista r m)     
                                

-- | Modifica um elemento numa dada 'Posicao'
--
-- __NB:__ Devolve a própria 'Matriz' se o elemento não existir.
atualizaPosicaoMatriz :: PosicaoMatriz -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz (r,c) x [] = []
atualizaPosicaoMatriz (r,c) x m = if ePosicaoMatrizValida (r,c) m   
                                     then atualizaIndiceLista r (atualizaIndiceLista c x (encontraIndiceLista r m)) m
                                  else m   