-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa2_2019li1g019 where

import LI11920
import Tarefa0_2019li1g019


-- * Testes

-- | Testes unitários da Tarefa 2.
--
-- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).
testesT2 :: [(Int,Jogada,Estado)]
testesT2 = [(1,(Movimenta D), (Estado [[Recta Boost 0,Recta Boost 0,Recta Terra 0,Rampa Lama 0 1,Rampa Boost 1 0],[Recta Terra 0,Recta Lama 0,Recta Boost 0,Recta Terra 0,Recta Lama 0]] [(Jogador 0 3.1 6 6 (Chao True)),(Jogador 1 5.2 3 5 (Chao True))])),(0,Dispara, (Estado [[Recta Terra 0, Recta Boost 0, Rampa Lama 0 2]][(Jogador 0 2.3 4 3 (Chao True))]))]

-- * Funções principais da Tarefa 2.

-- | Função que atualiza o Jogador no Mapa consoante a Jogada efetuada
atualizaJog :: Mapa -> Jogador -> Jogada -> Jogador

atualizaJog m (Jogador p di ve co es) Acelera | (verJogadorChao (Jogador p di ve co es) == True ) = Jogador p di ve co (Chao True)
                                               |otherwise =  Jogador p di ve co es

atualizaJog m (Jogador p di ve co es) Desacelera |(verJogadorChao (Jogador p di ve co es) == True )=Jogador p di ve co (Chao False)
                                                  |otherwise = Jogador p di ve co es

atualizaJog m (Jogador p di ve co es) Dispara   |(co>0) = Jogador p di ve (co - 1) es
                                                 |otherwise = Jogador p di ve co es

atualizaJog m (Jogador p di ve co es) (Movimenta C) |((verJogadorChao (Jogador p di ve co es) == True ) && p>0 ) = novoJogador m (Jogador p di ve co es ) (Jogador (p-1) di ve co es) 
                                                     |otherwise = Jogador p di ve co es 

atualizaJog m (Jogador p di ve co es) (Movimenta B) |((verJogadorChao (Jogador p di ve co es) == True ) && (p < (length m))) = Jogador (p + 1) di ve co es 
                                                     |otherwise = Jogador p di ve co es

atualizaJog m (Jogador p di ve co (Ar a i g)) (Movimenta D) |i >=(-75) =Jogador p di ve co (Ar a (i-15) g)
                                                                   |(i <(-75) && i >(-90)) = Jogador p di ve co (Ar a (-90) g)
                                                                   |otherwise = Jogador p di ve co (Ar a i g)

atualizaJog m (Jogador p di ve co (Ar a i g)) (Movimenta E) |i <=75 = Jogador p di ve co (Ar a (i+15)g)
                                                                    |(i >75 && i < 90) = Jogador p di ve co (Ar a 90 g)
                                                                    |otherwise = Jogador p di ve co (Ar a i g)                                          


-- | Função que averigua se um jogador se encontra no chão

verJogadorChao :: Jogador -> Bool
verJogadorChao (Jogador p di ve co (Chao True)) = True
verJogadorChao (Jogador p di ve co (Chao False)) = True
verJogadorChao _ = False 

-- | Caso a altura seja <= a 0.2,consegue trocar de pista e então temos Jogador novo, caso contrário morre ou fica no ar com altura da peça para onde vai e a inclinação da peça anterior
novoJogador :: Mapa -> Jogador -> Jogador -> Jogador
novoJogador m (Jogador p1 di1 ve1 co1 es1) (Jogador p2 di2 ve2 co2 es2) | (e - v) <= 0.2 = (Jogador p2 di2 ve2 co2 es2)
                                                                          | v > e  = Jogador p1 di1 ve1 co1 (Morto (1.0))
                                                                          | otherwise = (Jogador p2 di2 ve2 co2 (Ar v i 0))
                                                                    
                                                                          where
                                                                          v = calculaAturaJo (Jogador p2 di2 ve2 co2 es2) (encontraPosicaoMatriz(p2, floor(di2)) m) 
                                                                          e = calculaAturaJo (Jogador p1 di1 ve1 co1 es1) (encontraPosicaoMatriz(p1, floor(di1)) m )
                                                                          i = calculaInclinacaoJo(Jogador p1 di1 ve1 co1 es1) (encontraPosicaoMatriz(p1, floor(di1)) m)
 
-- | Calcula a Altura a que o jogador se encontra numa Peça
calculaAturaJo :: Jogador -> Peca -> Double
calculaAturaJo (Jogador p di ve co es) (Recta piso a) = (fromIntegral a)
calculaAturaJo (Jogador p di ve co es) (Rampa piso ai af) |(af > ai) = tan (atan (fromIntegral(af-ai))) * (di - fromIntegral (floor(di)))
                                                           |otherwise = tan(atan (fromIntegral(ai-af))) * (1- (di- fromIntegral (floor (di))))


-- | Calcula a Inclinação de um jogador numa determinada Peça
calculaInclinacaoJo :: Jogador -> Peca -> Double
calculaInclinacaoJo  (Jogador p di ve co es) (Recta piso a) = 0.0
calculaInclinacaoJo  (Jogador p di ve co es) (Rampa piso ai af) | (af > ai) = (atan (fromIntegral (af-ai))) * (fromIntegral (180) / (pi))   
                                                                | (af < ai) = (atan (fromIntegral (ai-af))) * (fromIntegral (180) / (pi))
                                                                | otherwise = 0.0


-- | Altera o solo para tipo "Cola"

colaOn :: Peca -> Peca
colaOn (Recta piso a) = (Recta Cola a)
colaOn (Rampa piso ai af) = (Rampa Cola ai af)

-- | Atualiza o mapa caso tenha sido disparada cola
mapaComCola :: Mapa -> Peca -> (Int, Double) -> Mapa
mapaComCola m  peça  (p,di) = atualizaPosicaoMatriz (p,floor (di)-1) (colaOn peça ) m

-- | Função que fornece a pista que o jogador está.
daPista :: Jogador -> Int
daPista (Jogador p di ve co es) = p

-- | Função que fornece a distância a que o jogador está da posição inicial
daDistancia :: Jogador -> Double 
daDistancia (Jogador p di ve co es) = di

-- | Com o mapa e o Jogador vamos conseguir descobrir a peça onde o jogador se encontra
daPeca:: Mapa -> Jogador -> Peca
daPeca m (Jogador p di ve co es) = encontraPosicaoMatriz (daPista (Jogador p di ve co es), floor(daDistancia (Jogador p di ve co es))) m  

--  | Função que com o índice do jogador pretendido e com o Jogador atualizado, vai colocar na lista de jogadores, a lista atualizada no índice n.
alteraJogador :: Int -> Jogador -> [Jogador] -> [Jogador]
alteraJogador 0 j (h:t) = (j:t)
alteraJogador n j (h:t) | n>0 = (h: alteraJogador  (n-1) j t)

-- ^| Efetua uma jogada.
-- ^ O identificador do 'Jogador' que efetua a jogada.;^ A 'Jogada' a efetuar.;^ O 'Estado' anterior. ;^ O 'Estado' resultante após o jogador efetuar a jogada.

jogada :: Int -> Jogada -> Estado -> Estado 
jogada  n jogadinha (Estado m js) |(jogadinha == Dispara)= Estado (mapaComCola m (daPeca m a) (daPista a,daDistancia a)) (alteraJogador n (atualizaJog m a jogadinha) js)
                                  |otherwise = Estado m (alteraJogador n (atualizaJog m a jogadinha) js)
                                  where  
                                    a = (encontraIndiceLista n js)



