-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2019li1g019 where

import LI11920
import Tarefa0_2019li1g019

type Tempo = Double


-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um par (/tempo/,/'Mapa'/,/'Jogador'/).
testesT4 :: [(Double,Mapa,Jogador)]
testesT4 = [(5.3, [[Recta Terra 0, Rampa Relva 0 1, Rampa Boost 1 0, Recta Lama 0][Recta Terra 0, Recta Relva 0, Recta Boost 0, Recta Lama 0]], (Jogador 1 2 3 3 (Chao True))),(17.0, [[Recta Terra 0, Rampa Relva 0 1, Recta Boost 1 , Recta Lama 1][Recta Terra 0, Recta Relva 0, Recta Boost 0, Rampa Lama 0]], (Jogador 2 1 3 3 (Chao False))),(8.4, [[Recta Terra 0, Rampa Relva 0 1, Rampa Boost 1 0, Recta Lama 0][Recta Terra 0, Recta Relva 0, Recta Boost 0, Recta Lama 0]], (Jogador 3 2 3 3 (Chao True))),(16.3, [[Recta Terra 0, Rampa Relva 0 1, Rampa Boost 1 0, Recta Lama 0][Recta Terra 0, Recta Relva 0, Recta Boost 0, Recta Lama 0]], (Jogador 1 2 3 3 (Ar (0.5)(2.0)(1.0))))(99.9, [[Recta Terra 0, Rampa Relva 0 1, Rampa Boost 1 0, Recta Lama 0][Recta Terra 0, Recta Relva 0, Recta Boost 0, Recta Lama 0]], (Jogador 1 2 3 3 (Morto (1.0))))]



-- |A partir de um Piso retorna um Double correspondente aos atritos predefinidos no enunciado:
-- |Terra 0.25 , Relva 0.75 , Lama 1.50, Boost -0.5 , cola 3.00.
atritos :: Piso -> Double
atritos Terra = 0.25
atritos Relva = 0.75
atritos Lama = 1.50
atritos Boost = -0.5
atritos Cola = 3.00


-- | Através da distância e da pista retorna o piso onde o jogador se encontra.
getPiso :: [Int] -> Int -> Pista -> Piso 
getPiso [x] d pista = pecaToPiso(getPisoAux x (floor d))
getPiso (x:xs) d pista = if (pista>0) then (getPiso xs d (pista-1)) else pecaToPiso(getPisoAux x (floor d))

-- | Com a pista já calculada descobre a peça em que está.
getPisoAux :: Pista -> Int -> Peca
getPisoAux [h] d = h
getPisoAux (h:t) d = if(d>0) then getPisoAux t (d-1) else h

-- | Dá o piso consoante a peça.
pecaToPiso :: Peca -> Piso
pecaToPiso (Recta x y) = x
pecaToPiso (Rampa x y z) = x


-- |Recebe o mapa para calcular o piso com a distância do jogador e a pista retornando o atrito (Double)
atrito :: Mapa -> Double -> Int -> Double
atrito m d pista = atritos (getPiso m d pista)


accelMota :: Double -> EstadoJogador -> Int
accelMota v x |(v<2 && (aceleraJogador x)) = 1
              |otherwise = 0



-- | Com um mapa utilizador posteriormente na função atrito, o Jogador com distância e velocidade e ainda o tempo, retorna a nova velocidade.
velocidadeChao :: Mapa -> Jogador -> Tempo -> Double
velocidadeChao m j t = (velocidadeJogador j)+((fromIntegral(accelMota (velocidadeJogador j) (estadoJogador j))-((atrito m (distanciaJogador j) (pistaJogador j))*(velocidadeJogador j)))*t) 

-- |Função para a nova velocidade não ser negativa.
novaVelocidadeChao :: Mapa -> Jogador -> Tempo -> Double
novaVelocidadeChao m j t = if((velocidadeChao m j t)<0) then 0 else (velocidadeChao m j t)

-- |Set da resistência do ar a 0.125
resistenciaAr :: Double
resistenciaAr = 0.125


-- | Recebe o jogador e o tempo, retornando a  nova velocidade quando esta no ar, sem ter em conta se é negativa
velocidadeAr :: Jogador -> Tempo -> Double
velocidadeAr j t = (velocidadeJogador j) - (resistenciaAr * (velocidadeJogador j)*t)

-- | Dá a nova velocidade que caso tenha sido negativa, fica 0.
novaVelocidadeAr :: Jogador -> Tempo -> Double
novaVelocidadeAr j t = if((velocidadeAr j t)<0) then 0 else (velocidadeAr j t)


-- | Set accelGravidade.
accelGravidade :: Double
accelGravidade = 1.0



-- | Nova velocidade causada pela gravidade do jogador,que e se o estado for "Ar" tira-se a gravidade atual
novaGravidade :: Jogador -> Tempo -> Double
novaGravidade (Jogador p d v c (Ar a i g)) t = g + (accelGravidade*t)

-- | Move o jogador no estado morto, se o timeout menos o tempo não for 0 decrementa, se for 0 o estado altera para "Chao false".
moveMorto :: Jogador -> Tempo -> Jogador
moveMorto j@(Jogador p d v c e) t = if(((timeoutJogador (estadoJogador j))-t)>0) then (Jogador p d v c (Morto (timeoutJogador (estadoJogador j))-t)) 
                                    else (Jogador p d v c (Chao False))


-- | Se a nova distância for menor que o inteiro + 1(ceiling), então não está no limite.
notLimitePeca :: Jogador -> Mapa -> Tempo -> Bool
notLimitePeca j m t = if((novaDistancia j m t )< fromIntegral(ceiling(distanciaJogador j))) then True
                      else False 

-- | Calcula a nova distância através da velocidade e do tempo para saber se muda ou não de peça 
novaDistancia :: Jogador -> Mapa -> Tempo -> Double
novaDistancia j m t = (distanciaJogador j) + ((velocidadeJogador j)*t)*cos((calculaInclinacao (encontraPeca j m))*(180 / pi ))

-- | Altera a distância e o estado de um jogador enquanto está no chão.
moveChao :: Jogador -> Mapa -> Tempo -> Jogador
moveChao j@(Jogador p d v c e ) m t = if (notLimitePeca j m t) then (Jogador p (novaDistancia j m t) v c e)
                                      else if(calculaInclinacao j (encontraNextPeca j m)  >= calculaInclinacao (encontraPeca j m)) then (Jogador p (novaDistancia j m t) v c e)
                                           else (Jogador p (novaDistancia j m t) v c (Ar  (alturaPeca (encontraPeca j m)) (calculaInclinacao (encontraPeca j m)) (0.0)))


-- |Calcula a inclinação da peça 
calculaInclinacao :: Peca -> Double
calculaInclinacao (Recta piso a) = 0.0
calculaInclinacao (Rampa piso ai af) | (af > ai) = (atan (fromIntegral (af-ai))) * (fromIntegral (180) / (pi))   
                                                                                           | (af < ai) = (atan (fromIntegral (ai-af))) * (fromIntegral (180) / (pi))
                                                                                           | otherwise = 0.0


-- |Encontra a próxima peça tendo a pista e a distância. 
encontraNextPeca :: Jogador -> Mapa -> Peca
encontraNextPeca (Jogador p d v c e) m = encontraPosicaoMatriz (p,((floor d)+(1.0))) m 

-- |Encontra a peça tendo a pista, distância e mapa.
encontraPeca :: Jogador -> Mapa -> Peca
encontraPeca (Jogador p d v c e) m = encontraPosicaoMatriz (p,(floor d)) m 

-- | Calcula a altura de uma peça.
alturaPeca :: Peca -> Double
alturaPeca (Recta p x) = fromIntegral(x)
alturaPeca (Rampa p x y) = fromIntegral(y)

-- | Verifica se quando está no ar bate no chão.
bateChao :: Jogador -> Tempo -> Mapa -> Bool
bateChao j@(Jogador p d v c (Ar a i g)) t m = if(notLimitePeca j m t) then intersetam((vetorPeca (encontraPeca j m)) ((Cartesiano (d-floor d) a), Cartesiano (novaVelocidadeAr j t) (novaGravidade j t)) )
                            else False
bateChao _ _ _ = False

-- | Vetor associado à direção da peça.
vetorPeca :: Peca -> Reta
vetorPeca (Recta p x) = ((Cartesiano 0 x),(Cartesiano 1 x))
vetorPeca (Rampa p ai af) = ((Cartesiano 0 ai),(Cartesiano 1 af))

-- | Com um ponto, devolve o primeiro elemnto, x.
xponto :: Ponto -> Double
xponto (Cartesiano x _) = x
xponto p = xponto (polarToCartesiano p)

-- | Com um Jogador, um mapa e o tempo determina a nova distância de um jogador que está no ar e bate no chão.
novaDistanciaBateCh :: Jogador -> Mapa -> Tempo  -> Double
novaDistanciaBateCh j@(Jogador p d v c (Ar a i g)) m t =  xponto (intersecao((vetorPeca (encontraPeca j m)) ((Cartesiano (d-floor d) a), Cartesiano (novaVelocidadeAr j t) (novaGravidade j t)) ))



moveAr :: Jogador -> Mapa -> Tempo -> Jogador
moveAr j@(Jogador p d v c e) m t = if (notLimitePeca j m t) 
							     then (Jogador p (novaDistancia j m t) v c e)
                                 else if (bateChao j t m) 
                                 	then if (normalizaAngulo( (inclinacaoJogador (estadoJogador j)) - (calculaInclinacao (encontraPeca j m))) >= 45) 
                                    	then (Jogador p (novaDistanciaBateCh j m t) v c (Morto (1.0)))  
                                        else (Jogador p (novaDistanciaBateCh j m t ) v c (Chao False))
                                    else (Jogador p (novaDistancia j m t) v c e)


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
acelera t m j@(Jogador p d v c (Chao True)) = (Jogador p d (novaVelocidadeChao m j t) c (Chao True)) 
acelera t m j@(Jogador p d v c (Chao False)) = (Jogador p d (novaVelocidadeChao m j t) c (Chao False))
acelera t m j@(Jogador p d v c (Ar a i g)) =  (Jogador p d (novaVelocidadeAr j t) c (Ar a i g))
acelera t m j@(Jogador p d v c (Morto timeout)) = (Jogador p d 0 c (Morto timeout))

-- | Altera a posição de 'Jogador', durante um determinado período de tempo.
move :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após se movimentar.
move t m j@(Jogador p d v c (Chao True)) = moveChao j m t
move t m j@(Jogador p d v c (Chao False)) = moveChao j m t
move t m j@(Jogador p d v c (Ar a i g)) = moveAr j m t
move t m j@(Jogador p d v c (Morto timeout)) = moveMorto j t


