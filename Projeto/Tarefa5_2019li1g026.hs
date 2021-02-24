-- | Este módulo define funções comuns da Tarefa 5 do trabalho prático.
module Main where

import LI11920
import Graphics.Gloss 
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Color 
import Graphics.Gloss.Juicy 
import Tarefa2_2019li1g026
import Tarefa4_2019li1g026
import Tarefa6_2019li1g026
import Tarefa1_2019li1g026
import Tarefa0_2019li1g026
import System.Random
import System.IO
import System.IO.Unsafe (unsafePerformIO)

-- * Introdução do Relatório
--
-- $introduçãoRelatório
--
--Na presente Tarefa 5 reside a responsabilidade da parte gráfica do nosso jogo, como também alguns outros aspetos. 
--Nesta tarefa, fizemos uso da biblioteca de Haskell denominada Gloss. Esta forneceu-nos as ferramentas necessárias 
--ao desenvolvimento de uma interface gráfica para o nosso jogo, sendo constituída não só pela parte do jogo 
--própriamente dito (movimento das bikes, implementação da cola, representação dos blocos pertencentes ao mapa), mas também um 
--menu onde estão disponíveis um modo de jogo contra um bot e um modo de 2 jogadores.O nosso grupo decidiu denominar a nossa versão 
--do jogo de 'Yike Bikes Race'.
--Uma pequena nota: O nome do jogo revela o que no fundo é o jogo, sendo este de corridas de yike bikes.

-- * Objetivos da Tarefa
--
-- $objetivosTarefa
--
--Nesta Tarefa, nós optamos por remodelar toda a parte gráfica do jogo, criando imagens e também adaptando algumas, através 
--do Photoshop. O tema escolhido para o nosso jogo é uma corrida de yike bikes. Posto isto, o fundo é composto 
--por um homem a andar numa yike bike sendo que no momento da corrida fazemos alusão à nossa série preferida: "The Simpson´s".
--Os tipos de piso que compõem o mapa têm todos texturas diferentes, sendo a cola representada por uma textura cinza,
--a terra por uma castanha, a lama por uma tonalidade mais escura do que o castanho, a relva representada pela cor verde e o 
--boost por uma textura de asfalto com umas setas impressas.
--Os jogadores são da nossa autoria, criados à mão por nós,apelando à nossa criatividade.
--Para corrermos todo este código, usamos uma outra biblioteca, própria para desenvolver com facilidade 
--protótipos de jogos, chamada Gloss.Interface.Pure.Game. Esta permite criar a janela na qual o jogo vai correr, 
--e desenvolver as funçoes que vão representar a passagem do tempo, a reação às várias teclas, e a função que vai 
--desenhar o estado.


-- | Função principal da Tarefa 5.
--
-- __NB:__ Esta Tarefa é completamente livre. Deve utilizar a biblioteca <http://hackage.haskell.org/package/gloss gloss> para animar o jogo, e reutilizar __de forma completa__ as funções das tarefas anteriores.

main :: IO ()
main = do 
        play dm                   -- janela onde vai ocorrer o jogo
             black                -- cor de fundo
             fr                   -- frame rate
             estadoJogoInicial    -- estado inicial
             desenhaEstadoGloss   -- desenha estado do jogo
             reageEvento          -- reage a um evento
             reageTempo           -- reage ao passar do tempo


-- * Funções necessárias para carregar as imagens essenciais ao funcionamento do jogo.

-- | Função que permite carregar uma imagem através do seu path.
png :: FilePath -> Picture
png fname = maybe (text "PNG ERROR") id (unsafePerformIO $ loadJuicyPNG fname)  

finish :: Picture
finish = png "finish.png"

ganhaste :: Picture
ganhaste = png "ganhaste.png"

botganhou :: Picture
botganhou = png "botganhou.png"

listacontrolo1 :: Picture
listacontrolo1 = png "listacontrolos1.png"

listacontrolo2 :: Picture
listacontrolo2 = png "listacontrolos2.png"

jogador1ganhou :: Picture
jogador1ganhou = png "jogador1ganhou.png"

jogador2ganhou :: Picture
jogador2ganhou = png "jogador2ganhou.png"

mvazia :: Picture
mvazia = png "MVazia.png"

comecajogopvp :: Picture
comecajogopvp = png "COMECAJOGOPVP.png"

comecajogo :: Picture
comecajogo = png "COMECAJOGO.png"

entsair :: Picture
entsair = png "ENTSAIR.png"

playervsplayer :: Picture
playervsplayer = png "PLAYERVSPLAYER.png"

sair :: Picture
sair = png "SAIR.png"

vscom :: Picture
vscom = png "VSCOM.png"

menupausa :: Picture
menupausa = png "menupausa.png"

entstart :: Picture
entstart = png "ENTSTART.png"

fundo :: Picture
fundo = png "fundo.png"

inicio :: Picture
inicio = png "INICIO.png"

corredor1 :: Picture
corredor1 = Scale (0.8) (0.8) (png "corredor.png")

corredor2 :: Picture
corredor2 = Scale (0.8) (0.8) (png "corredor2.png")

corredormorto :: Picture
corredormorto = Scale (0.8) (0.8) (png "corredormorto.png")

terra :: Picture
terra = png "terra.png"

cola :: Picture
cola = png "cola.png"

relva :: Picture
relva = png "relva.png"

lama :: Picture
lama = png "lama.png"

boost :: Picture
boost = png "boost.png"

simpsonsky :: Picture
simpsonsky = png "simpsonsky.png"

plateia1 :: Picture
plateia1 = Translate (comprimento+410) (largura+100) (png "simpsonsplateia.png")

plateia2 :: Picture
plateia2 = Translate (comprimento+410) (largura-180) (png "simpsonsplateia2.png")

boss :: Picture
boss = Translate (comprimento-150) (largura+180) (png "felix.png")

-- * Definições essenciais para a Tarefa funcionar.

-- | Definição da frame rate.
fr :: Int
fr = 13

-- | Definição da janela de jogo.
dm :: Display
dm = FullScreen

-- * Definição de tipos fundamentais para o concretizar da Tarefa.
-- | Definição de Menu.
type Menu = Int 
-- | Definição de EstadoGloss.
type EstadoGloss = (Menu,Estado)

-- * Funções que irão desenhar o nosso EstadoGloss
-- | Função que define através de um EstadoGloss a imagem a aparecer no ecrã.
desenhaEstadoGloss :: EstadoGloss -> Picture
desenhaEstadoGloss (8,e1) = case vitoriavscom e1 of 'W' -> definemenu (12,e1)
                                                    'L' -> definemenu (13,e1)
                                                    'D' -> desenhaEstadoG (8,e1)
desenhaEstadoGloss (9,e1) = case vitoriavscom e1 of 'W' -> definemenu (10,e1)
                                                    'L' -> definemenu (11,e1)
                                                    'D' -> desenhaEstadoG (9,e1) 
desenhaEstadoGloss (m,e1) = desenhaEstadoG (m,e1)                                                                                                                
                              
-- | Função auxiliar da anterior que imprime mapa, jogadores, bancada, fundo e bandeira de finish no modo de jogo.
desenhaEstadoG :: EstadoGloss -> Picture
desenhaEstadoG (menu,(Estado m (x:xs))) | (menu==8) = Pictures (simpsonsky:plateia1:plateia2:boss:(desenhaFinish finish):(desenhaFinish2 finish):(desenhaEstado (Estado m (x:xs))))
                                        | (menu==9) = Pictures (simpsonsky:plateia1:plateia2:boss:(desenhaFinish finish):(desenhaFinish2 finish):(desenhaEstado (Estado m (x:xs))))
                                        | otherwise = definemenu  (menu,(Estado m (x:xs)))


-- * Definição de estados iniciais fundamental ao arranque do jogo.
-- | EstadoGloss inicial que será logo impresso no ecrã mal seja executado.
estadoJogoInicial :: EstadoGloss
estadoJogoInicial = (0,estadoInicial)

-- | Estado de jogo inicial a ser implementado no EstadoGloss inicial.
estadoInicial :: Estado
estadoInicial = (Estado (gera 2 10 7) [Jogador 0 0 0 5 (Chao True),Jogador 1 0 0 5 (Chao False)])  --arranjar funçao que devolva um numero random

-- * Funções Auxiliares.
-- | Definição de comprimento e largura.
comprimento :: Float
comprimento = (-425)

largura :: Float
largura = 60

-- | Funções que determinam a inclinação de uma peça.
inclination :: Int -> Int -> Float
inclination x y = (atan (realToFrac(y-x))) * 57.29577951308230876798155

inclina :: Peca -> Float
inclina (Recta tipo h) = 0
inclina (Rampa tipo h1 h2) = inclination h1 h2

-- | Função que retira o mapa de um Estado.
mapadoestado :: Estado -> Mapa
mapadoestado (Estado m j) = m

-- | Função que transforma um tipo "Maybe Jogada" em "Jogada".
tranformaMJ :: Maybe Jogada -> Jogada
tranformaMJ (Just a) = a 

-- | Função que retira a lista de jogadores de um Estado.
listadoestado :: Estado -> [Jogador]
listadoestado (Estado m j) = j

-- | Função que dados dois jogadores, retorna uma lista de ambos.
fazlistaJ :: Jogador -> Jogador -> [Jogador]
fazlistaJ j p = [j,p]

-- | Função que junta uma imagem numa lista de imagens e coloca-a no início.
juntaImagem :: Picture -> [Picture] -> [Picture]
juntaImagem x l = x:l

-- * Funções que culminam no desenho do Estado do jogo.

-- | Definição do lado de uma Peca.
ladoBloco :: Float
ladoBloco = 90

-- | Função principal que desenha o Estado atual do jogo.
desenhaEstado :: Estado -> [Picture]
desenhaEstado (Estado m players) = (desenhaMapa m comprimento largura) ++ (desenhaJogadores m players)

-- | Função que desenha o mapa do Estado, a ser implementada na "desenhaEstado"
desenhaMapa :: Mapa -> Float -> Float -> [Picture]
desenhaMapa [] _ _ = []
desenhaMapa (h:t) x y = (desenhaPista h x y) ++ (desenhaMapa t x (y-ladoBloco))

-- | Função que desenha uma pista que será invocada na função anterior de modo a desenhar o mapa.
desenhaPista :: Pista -> Float -> Float -> [Picture]
desenhaPista [] _ _ = []
desenhaPista (h:t) x y = juntaImagem (desenhaBloco h x y) (desenhaPista t (x+ladoBloco) y)

-- | Função que desenha cada peça do mapa.
desenhaBloco :: Peca -> Float -> Float -> Picture
desenhaBloco (Recta Terra h) x y = Translate x y terra
desenhaBloco (Recta Relva h) x y = Translate x y relva                                                
desenhaBloco (Recta Lama h) x y = Translate x y lama                                                
desenhaBloco (Recta Boost h) x y = Translate x y boost                                                 
desenhaBloco (Recta Cola h) x y = Translate x y cola                                               
desenhaBloco (Rampa Terra h1 h2) x y = Translate x y ({--Rotate (inclination h1 h2)--} terra)                                               
desenhaBloco (Rampa Relva h1 h2) x y = Translate x y ({--Rotate (inclination h1 h2)--} relva)                                                 
desenhaBloco (Rampa Lama h1 h2) x y = Translate x y ({--Rotate (inclination h1 h2)--} lama)                                                 
desenhaBloco (Rampa Boost h1 h2) x y = Translate x y ({--Rotate (inclination h1 h2)--} boost)            
desenhaBloco (Rampa Cola h1 h2) x y = Translate x y ({--Rotate (inclination h1 h2)--} cola)  

-- | Função que desenha a bandeira de Finish na primeira pista.
desenhaFinish :: Picture -> Picture
desenhaFinish x = Translate (comprimento+825) (largura-90) x

-- | Função que desenha a bandeira de Finish na segunda pista.
desenhaFinish2 :: Picture -> Picture
desenhaFinish2 x = Translate (comprimento+825) largura x

-- | Função que desenha os jogadores de modo aque seja invocada na função "desenhaEstado".
desenhaJogadores :: Mapa -> [Jogador] -> [Picture]
desenhaJogadores _ [] = []
desenhaJogadores m (h:t) = juntaImagem (desenhaJogador1 m h) (desenhaJogadores2 m t)

desenhaJogadores2 :: Mapa -> [Jogador] -> [Picture]
desenhaJogadores2 m [] = []
desenhaJogadores2 m (h:t) = juntaImagem (desenhaJogador2 m h) (desenhaJogadores2 m t)

-- | Função que desenha o jogador 1 e o coloca no devido local.
desenhaJogador1 :: Mapa -> Jogador -> Picture
desenhaJogador1 m (Jogador p d v c (Chao True)) = Translate (((realToFrac d)*90)+comprimento) (largura-(90*(fromIntegral p))) ({--Rotate (inclina (wherePlayer (Jogador p d v c (Chao True)) m))--} corredor1)
desenhaJogador1 m (Jogador p d v c (Chao False)) = Translate (((realToFrac d)*90)+comprimento) (largura-(90*(fromIntegral p))) ({--Rotate (inclina (wherePlayer (Jogador p d v c (Chao False)) m))--} corredor1)                                      
desenhaJogador1 m (Jogador p d v c (Morto h)) = Translate (((realToFrac d)*90)+comprimento) (largura-(90*(fromIntegral p))) corredormorto
desenhaJogador1 m (Jogador p d v c (Ar a i g)) = Translate (((realToFrac d)*90)+comprimento) (largura-(90*(fromIntegral p))) corredor1

-- | Função que desenha o jogador 2 e o coloca no devido local.
desenhaJogador2 :: Mapa -> Jogador -> Picture
desenhaJogador2 m (Jogador p d v c (Chao True)) = Translate (((realToFrac d)*90)+comprimento) (largura-(90*(fromIntegral p))) ({--Rotate (inclina (wherePlayer (Jogador p d v c (Chao True)) m))--} corredor2)
desenhaJogador2 m (Jogador p d v c (Chao False)) = Translate (((realToFrac d)*90)+comprimento) (largura-(90*(fromIntegral p))) ({--Rotate (inclina (wherePlayer (Jogador p d v c (Chao False)) m))--} corredor2)                                      
desenhaJogador2 m (Jogador p d v c (Morto h)) = Translate (((realToFrac d)*90)+comprimento) (largura-(90*(fromIntegral p))) corredormorto
desenhaJogador2 m (Jogador p d v c (Ar a i g)) = Translate (((realToFrac d)*90)+comprimento) (largura-(90*(fromIntegral p))) corredor2

-- * Função que define os menus, retornando a imagem a imprimir no ecrã, através de um EstadoGloss.
definemenu :: EstadoGloss -> Picture
definemenu (x,e) = case x of 0 -> inicio
                             1 -> entstart
                             2 -> vscom
                             3 -> playervsplayer
                             4 -> sair
                             5 -> entsair
                             6 -> comecajogo
                             7 -> comecajogopvp
                             8 -> mvazia
                             9 -> mvazia
                             10 -> jogador1ganhou
                             11 -> jogador2ganhou
                             12 -> ganhaste
                             13 -> botganhou
                             14 -> listacontrolo1 
                             15 -> listacontrolo2 
                             16 -> menupausa
                             17 -> menupausa
                             18 -> inicio
                             19 -> inicio 
                             20 -> inicio
                             21 -> inicio
                             22 -> inicio
                             23 -> inicio
                             24 -> inicio
                             25 -> inicio 

-- * Funções que reagem aos eventos efectuados pelo Homem e ao tempo.
-- | Função que transforma um EstadoGloss noutro devido a uma ação do utilizador.
reageEvento :: Event -> EstadoGloss -> EstadoGloss
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (1,e)= (2,e)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (2,e)= (6,e)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (6,e)= (14,e)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (14,e)= (8,e)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (2,e)= (2,e)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (2,e) = (3,e)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (3,e)=(15,e)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (15,e)= (7,e)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (7,e)= (9,e)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (3,e)= (2,e)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (3,e) = (4,e)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (4,e)= (5,e)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (4,e)= (3,e)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (4,e) = (4,e)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (5,e)= (50,e) 
reageEvento (EventKey (SpecialKey KeyEsc) Down _ _) (x,e) = case x of 10 -> (2,estadoInicial)
                                                                      11 -> (2,estadoInicial)
                                                                      12 -> (2,estadoInicial)
                                                                      13 -> (2,estadoInicial)
                                                                      8 -> (2,estadoInicial)
                                                                      9 -> (2,estadoInicial)
                                                                      otherwise -> (x,e)
reageEvento (EventKey (SpecialKey KeySpace) Down _ _) (x,e)= case x of 8 -> (16,e)
                                                                       9 -> (17,e)
                                                                       otherwise -> (x,e)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (x,e)= case x of 16 -> (8,e)
                                                                       17 -> (9,e)
                                                                       otherwise -> (x,e)                                                                        
----------------------------------VSCOM
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (8,e) = (8,(jogada 0 (Movimenta C) e))
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (8,e) = (8,(jogada 0 (Movimenta B) e))
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (8,e) = (8,(jogada 0 (Movimenta E) e))
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (8,e) = (8,(jogada 0 (Movimenta D) e))
reageEvento (EventKey (Char 'a') Down _ _) (8,e) = (8,(jogada 0 (Acelera) e))
reageEvento (EventKey (Char 'd') Down _ _) (8,e) = (8,(jogada 0 (Desacelera) e))
reageEvento (EventKey (Char 'g') Down _ _) (8,e) = (8,(jogada 0 (Dispara) e))
reageEvento _ (8,e) = (8,e)
---------------------------------PLAYERVSPLAYER
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (9,e) = (9,(jogada 0 (Movimenta C) e))
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (9,e) = (9,(jogada 0 (Movimenta B) e))
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (9,e) = (9,(jogada 0 (Movimenta E) e))
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (9,e) = (9,(jogada 0 (Movimenta D) e))
reageEvento (EventKey (Char 'p') Down _ _) (9,e) = (9,(jogada 0 (Acelera) e))
reageEvento (EventKey (Char 'l') Down _ _) (9,e) = (9,(jogada 0 (Desacelera) e))
reageEvento (EventKey (Char 'm') Down _ _) (9,e) = (9,(jogada 0 (Dispara) e))
reageEvento (EventKey (Char 'w') Down _ _) (9,e) = (9,(jogada 1 (Movimenta C) e))
reageEvento (EventKey (Char 's') Down _ _) (9,e) = (9,(jogada 1 (Movimenta B) e))
reageEvento (EventKey (Char 'a') Down _ _) (9,e) = (9,(jogada 1 (Movimenta E) e))
reageEvento (EventKey (Char 'd') Down _ _) (9,e) = (9,(jogada 1 (Movimenta D) e))
reageEvento (EventKey (Char 'z') Down _ _) (9,e) = (9,(jogada 1 (Acelera) e))
reageEvento (EventKey (Char 'x') Down _ _) (9,e) = (9,(jogada 1 (Desacelera) e))
reageEvento (EventKey (Char 'c') Down _ _) (9,e) = (9,(jogada 1 (Dispara) e))
reageEvento _ (9,e) = (9,e)
---------------------------------
reageEvento _ (x,e) = (x,e)

-- | Função que transforma um EstadoGloss noutro devido à ação do tempo.
reageTempo :: Float -> EstadoGloss -> EstadoGloss
reageTempo n (0,e) = (18,e)
reageTempo n (18,e) = (19,e)
reageTempo n (19,e) = (20,e)
reageTempo n (20,e) = (21,e)
reageTempo n (21,e) = (22,e)
reageTempo n (22,e) = (23,e)
reageTempo n (23,e) = (24,e)
reageTempo n (24,e) = (25,e)
reageTempo n (25,e) = (1,e) 
reageTempo n (m,e) | (m==8) && ((bot 1 (Estado (mapadoestado (e)) (atualizaJogadorLista 1 (passo (realToFrac n) (mapadoestado(e)) (indPlayer 1 (listadoestado (e)))) (listadoestado(e)))))==(Nothing)) = (8,e)
                   | (m==8) && otherwise = (8,(jogada 1 (tranformaMJ (bot 1 (Estado (mapadoestado (e)) (atualizaJogadorLista 1 (passo (realToFrac n) (mapadoestado(e)) (indPlayer 1 (listadoestado (e)))) (listadoestado(e))))))(Estado (mapadoestado (e)) (atualizaJogadorLista 1 (passo (realToFrac n) (mapadoestado(e)) (indPlayer 1 (listadoestado (e)))) (listadoestado(e))))))
                   | (m==9) = (9,(Estado (mapadoestado(e)) (fazlistaJ (passo (realToFrac n) (mapadoestado(e)) (indPlayer 0 (listadoestado (e)))) (passo (realToFrac n) (mapadoestado(e)) (indPlayer 1 (listadoestado (e)))))))
                   |otherwise = (m,e)

-- *
-- | Função que determina se algum dos jogadores já ganhou ou perdeu de modo a dar indicação para qual menu imprimir.
vitoriavscom :: Estado -> Char
vitoriavscom (Estado m [Jogador p d v c e,Jogador p1 d1 v1 c1 e1]) | (d==10) = 'W'
                                                                   | (d1==10) = 'L'
                                                                   | otherwise = 'D' 


-- * Conclusão do Relatório
--
-- $conclusãoRelatório
--
--Nesta tarefa o resultado pretendido foi parcialmente obtido: um executável que funciona sem nenhum erro que impeça 
--a sua utilização, na qual o utilizador tem várias opções, tem menus diferentes, e modos de jogo diferentes. 
--No entanto, não conseguimos pôr os corredores a andar, conseguindo apenas com que ele mude de pista, tanto no 
--modo de jogo contra o bot como no modo player vs player.