-- | Este módulo define funções genéricas sobre vetores e matrizes, que serão úteis na resolução do trabalho prático.
module Tarefa0_2019li1g019 where

-- * Funções não-recursivas.

-- | Um ponto a duas dimensões dado num referencial cartesiado (distâncias aos eixos vertical e horizontal)
--
-- <<http://li1.lsd.di.uminho.pt/images/cartesiano.png cartesisano>>
-- , ou num referencial polar (distância à origem e ângulo do respectivo vector com o eixo horizontal).
--
-- <<http://li1.lsd.di.uminho.pt/images/polar.png polar>>
data Ponto = Cartesiano Double Double | Polar Double Angulo deriving Show

-- | Um ângulo em graus.
type Angulo = Double

-- ** Funções sobre vetores

-- | Um 'Vetor' na representação escalar é um 'Ponto' em relação à origem.
type Vetor = Ponto
-- ^ <<http://li1.lsd.di.uminho.pt/images/vetor.png vetor>>

-- *** Funções gerais sobre 'Vetor'es.

-- | Soma dois 'Vetor'es.
somaVetores :: Vetor -> Vetor -> Vetor
somaVetores v1 v2 = (Cartesiano (x1 + x2) (y1 + y2))
    where
    x1 = posx v1
    x2 = posx v2
    y1 = posy v1
    y2 = posy v2

posx :: Vetor -> Double
posx (Cartesiano x y) = x
posx (Polar r a) = r * cos a

posy :: Ponto -> Double
posy (Cartesiano x y) = y
posy(Polar r a) = r * sin a

-- | Subtrai dois 'Vetor'es.
subtraiVetores :: Vetor -> Vetor -> Vetor
subtraiVetores v1 v2 = Cartesiano (x1 - x2) (y1 - y2)
    where 
        x1 = posx v1
        x2 = posx v2
        y1 = posy v1
        y2 = posy v2

-- | Multiplica um escalar por um 'Vetor'.
multiplicaVetor :: Double -> Vetor -> Vetor
multiplicaVetor a v2 = Cartesiano (a * x) (a * y)
    where 
        x = posx v2
        y = posy v2

-- ** Funções sobre rectas.

-- | Um segmento de reta é definido por dois pontos.
type Reta = (Ponto,Ponto)

-- | Testar se dois segmentos de reta se intersetam.
--
-- __NB:__ Aplique as equações matemáticas bem conhecidas, como explicado por exemplo em <http://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.
intersetam :: Reta -> Reta -> Bool
intersetam (p1,p2) (p3,p4) = 0 <= ta && ta <= 1 && 0 <= tb && tb <= 1
    where 
        x1 = posx p1
        x2 = posx p2
        x3 = posx p3
        x4 = posx p4
        y1 = posx p1
        y2 = posx p2
        y3 = posy p3
        y4 = posx p4
        ta = ((y3-y4) * (x1-x3) + (x4 - x3) * (y1 - y3)) / ((x4-x3) * (y1-y2) - (x1-x2) * (y4-y3))
        tb = ((y1-y2) * (x1-x3) + (x2-x1) * (y1-y3))/(x4-x3) * (y1 - y2) - (x1 - x2) * (y4-y3)


-- | Calcular o ponto de intersecao entre dois segmentos de reta.
--
-- __NB:__ Aplique as equações matemáticas bem conhecidas, como explicado por exemplo em <http://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.
intersecao :: Reta -> Reta -> Ponto
intersecao x@(a, b) y@(c, d) | intersetam x y = let (ta, tb) = aux x y in  somaVetores a (multiplicaVetor ta (subtraiVetores b a))
                             | otherwise = error "As retas não se intersetam"

aux :: Reta -> Reta -> (Double, Double)
aux (a, b) (c, d) = (ta, tb) 
    where
        (Cartesiano x1 y1) = polarToCartesiano a
        (Cartesiano x2 y2) = polarToCartesiano b
        (Cartesiano x3 y3) = polarToCartesiano c
        (Cartesiano x4 y4) = polarToCartesiano d
        ta = ((y3 - y4) * (x1 - x3) + (x4 - x3) * (y1 - y3)) / ((x4 - x3) * (y1 - y2) - (x1 - x2) * (y4 - y3))
        tb = ((y1 - y2) * (x1 - x3) + (x2 - x1) * (y1 - y3)) / ((x4 - x3) * (y1 - y2) - (x1 - x2) * (y4 - y3))

polarToCartesiano :: Ponto -> Ponto
polarToCartesiano (Cartesiano x y) = (Cartesiano x y)
polarToCartesiano (Polar r a) =(Cartesiano x y)
                where 
                    x=r*cos(a*pi/180)
                    y=r*sin (a* pi/180)
-- ** Funções sobre listas

-- *** Funções gerais sobre listas.
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | Verifica se o indice pertence à lista.
--
-- __Sugestão:__ use a função 'length' que calcula tamanhos de listas
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido i l =  i < length l && i >= 0

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
dimensaoMatriz m = if length (head m) == 0 then (0,0) else (length m, length (head m))

-- | Verifica se a posição pertence à matriz.
ePosicaoMatrizValida :: PosicaoMatriz -> Matriz a -> Bool 
ePosicaoMatrizValida (l,c) m | l >= 0 && c >= 0 = (l < length m) && (c < length (head m))
                             | otherwise = False
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
normalizaAngulo x | x > 360 = normalizaAngulo (x - 360)
                  | x < 0 = normalizaAngulo (x + 360)
                  | otherwise = x

-- ** Funções sobre listas.

-- | Devolve o elemento num dado índice de uma lista.
--
-- __Sugestão:__ Não use a função (!!) :: [a] -> Int -> a :-)
encontraIndiceLista :: Int -> [a] -> a
encontraIndiceLista i l | (eIndiceListaValido i l == True) = l!!i
                          | (eIndiceListaValido i l == False) = error "Elemento não pertence à lista"

-- | Modifica um elemento num dado índice.
--
-- __NB:__ Devolve a própria lista se o elemento não existir.
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista i x l | (eIndiceListaValido i l == True) = take i l ++ [x] ++ drop (i + 1) l
-- "take i l" para returnar o prefixo da lista até ao índice i; "drop (i + 1) l" para returnar o sufixo da lista l, a partir do índice i+1
                            | (eIndiceListaValido i l == False) = l

-- ** Funções sobre matrizes.

-- | Devolve o elemento numa dada 'Posicao' de uma 'Matriz'.
encontraPosicaoMatriz :: PosicaoMatriz -> Matriz a -> a
encontraPosicaoMatriz (l,c) m 
 | (l < length m) && (c < length (head m)) && (l>=0) && (c>=0) = encontraIndiceLista c (encontraColunaMatriz l m)
 | otherwise = error "Posição não válida"

-- | Devolve a linha de uma matriz, dado o valor da posição da coluna
encontraColunaMatriz :: Int -> Matriz a -> [a]
encontraColunaMatriz 0 m = head m
encontraColunaMatriz x m = encontraColunaMatriz (x-1) (tail m)
-- | Modifica um elemento numa dada 'Posicao'
--
-- __NB:__ Devolve a própria 'Matriz' se o elemento não existir.
atualizaPosicaoMatriz :: PosicaoMatriz -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz (l,c) e m 
 | (l < length m) && (c < length (head m)) = take l m ++ [atualizaIndiceLista c e (encontraColunaMatriz l m)] ++ drop (l+1) m
 | otherwise = m

