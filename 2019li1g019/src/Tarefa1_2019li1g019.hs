-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa1_2019li1g019 where

import LI11920
import System.Random

-- * Testes

-- | Testes unitários da Tarefa 1.
--
-- Cada teste é um triplo (/número de 'Pista's/,/comprimento de cada 'Pista' do 'Mapa'/,/semente de aleatoriedades/).
testesT1 :: [(Int,Int,Int)]
testesT1 = [(2,5,9),(1,1,1),(6,4,6),(4,3,7),(9,10,4),(0,3,4)]

-- * Funções pré-definidas da Tarefa 1.


geraAleatorios :: Int -> Int -> [Int]
geraAleatorios n seed = take n (randomRs (0,9) (mkStdGen seed))

-- * Funções principais da Tarefa 1.

-- |  Recebendo um Int e o Piso anterior, conseguimos identificar o piso em causa dependendo do Int
saberPiso :: Int -> Piso -> Piso
saberPiso n  p | (n==0 ||n==1) = Terra
               | (n==2||n==3) = Relva 
               | (n==4) = Lama 
               | (n==5) = Boost 
               |otherwise = p

-- | Para saber a peça, esta função recebe um inteiro e a peça anterior. Com estes valores determina a peça seguinte com uma deteminada altura, caso seja uma Recta ou uma Rampa

saberPeca :: Int ->Piso -> Peca -> Peca
saberPeca g p (Rampa _ i f)  | (g==0 || g==1) = Rampa p f (f+(g+1))
                             | (g==2 || g==3 || g==4 || g==5) = if (f==0) then Recta p 0
                                                                else  if ((f-(g-1)) >= 0) then Rampa p f (f-(g-1))
                                                                else Rampa p f 0
                             | otherwise = Recta p f

saberPeca g p (Recta _ f)  | (g==0 || g==1) = Rampa p f (f+(g+1))
                           | (g==2 || g==3 || g==4 || g==5) = if (f==0) then Recta p 0
                                                              else if ((f-(g-1)) >= 0) then Rampa p f (f-(g-1))
                                                                else Rampa p f 0 
                           | otherwise = Recta p f


-- | Função que gera uma lista de números inteiros através da funçãoo geraAleatorios com n(numero de pistas*comprimento*2)
listaAleatorios :: Int -> Int -> Int -> [Int]
listaAleatorios n c s = geraAleatorios(n*c*2) s


-- | "listaAPares" agrupa 2 inteiros de cada vez, originando uma lista de pares de inteiros.

listaAPares :: [Int]->[(Int,Int)]
listaAPares [] =[]
listaAPares (h:x:t) = (h,x) : listaAPares t 


-- | Conhecidos os elementos necessários para a criação de uma pista (Comprimento, Peca, Piso e uma lista de pares de inteiros, o primeiro de cada par dá o Piso e o segundo dá a Peca) e usando a recursividade obtemos uma lista de Peca que é uma Pista.
geraPista :: Int -> Peca -> Piso-> [(Int,Int)]  -> [Peca]
geraPista 0 p pi l = [] 
geraPista c p pi ((x,y):t) = (saberPeca y (saberPiso x pi) p) : geraPista (c-1) (saberPeca y (saberPiso x pi) p) (saberPiso x pi) t


-- | Gera o mapa tendo em conta o nº de pistas (Int) ,comprimento(Int) e uma lista de pares de Inteiros, escreve (Recta Terra 0) como 1º elemento e envia para o "geraPista" o valor do (comprimento-1), a peça anterior e o take (c-1) da lista a pares.  
gerarMapa :: Int -> Int ->[(Int,Int)]-> [[Peca]]
gerarMapa 0 c l = []
gerarMapa n c l = ((Recta Terra 0) : geraPista (c-1) (Recta Terra 0) (Terra) (take (c-1) l)) : gerarMapa (n-1) c (drop (c-1) l)

-- | Função principal que vai gerar um mapa com os parâmetros dados, utilizando as funções auxiliares acima.

gera :: Int -> Int -> Int -> Mapa
gera 0 c s = []
gera n c s = (gerarMapa n c (listaAPares (listaAleatorios n (c-1) s)))