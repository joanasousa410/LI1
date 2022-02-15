-- | Este módulo define funções comuns da Tarefa 3 do trabalho prático.
module Tarefa3_2019li1g019 where

import LI11920

{- | 
	<Relatório> : Tarefa 3
	-Introdução:
		A Tarefa 3 faz parte da primeira parte deste projeto prático da Unidade Curricular Laboratórios de Informática 1.
		O seu principal objetivo consiste na descontrução de um Mapa numa sequência de Instruções. 
		Devido a isto, fomos capazes de adquirir conhecimentos sobre a linguagem de Haskell e um pouco sobre o que acontece por detrás 
	de um jogo de computador.

	-Objetivos: 
		Inicialmente, decidimos transformar cada peça numa Instrução pois, deste modo,começariamos por uma etapa mais simples de forma a estruturar
	o nosso pensamento. No caso de a peça ser "Recta", a Instrução correspondente será "Anda". Do mesmo modo que "Rampa"
	será "Sobe"  se a altura final é maior que a altura inicial, e "Desce" se a altura inicial é maior que a altura final. Conseguindo assim 
	avançar com a resolução desta Tarefa.
		De seguida, visto que esta tarefa já não necessitava que a primeira peça de cada pista fosse "Recta Terra 0" como as tarefas anteriormente 
	trabalhadas, desenvolvemos uma função que retirava a primeira peça de cada pista. Posteriormente, tentamos ver as peças iguais seguidas e
	conseguimos agrupá-las, o que resultou numa lista de Instruções. Com esta lista de Instruções transformamos apenas em Instruções. 
	
	-Discussão e conclusão:
		Em suma, penso que 2 das 3 fases desta tarefa foram bem sucessidas, tendo em conta que o resultado foi o pretendido.
		Encontramos alguma dificuldade em fazer os padrões verticais, não concluindo desta forma a Tarefa 3.-}

		
-- * Testes

-- | Testes unitários da Tarefa 3.
--
-- Cada teste é um 'Mapa'.
testesT3 :: [Mapa]
testesT3 = [[[Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0,Recta Terra 0,Recta Terra 0,Rampa Lama 0 2,Rampa Lama 2 0]], [[Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Terra 0],[Recta Terra 0,Rampa Terra 0 1,Recta Terra 1,Rampa Terra 1 0]],[[Recta Terra 0,Recta Boost 0],[Recta Terra 0,Recta Boost 0],[Recta Terra 0,Recta Terra 0]]]
-- * Funções principais da Tarefa 3.

-- | Saber que de "Recta" passa a "Anda" e de "Rampa" para "Sobe ou Desce"
saberSobe_Desce_Anda :: Peca -> Int -> Instrucao
saberSobe_Desce_Anda (Recta piso a) pista = Anda [pista] piso
saberSobe_Desce_Anda (Rampa piso ai af) pista = if (af > ai) then Sobe [pista] piso (af-ai)
                                    else Desce [pista] piso (ai-af)

-- | Retira a primeira Peca de cada Pista (Recta Terra 0)
tiraPrimeira :: Mapa -> Mapa
tiraPrimeira [] =[]
tiraPrimeira ((x:xs):t) = (xs:tiraPrimeira t)

-- | Função que apenas passa um Mapa para Instruções de Sobe Desce e Anda
padrao :: Mapa -> Int -> Instrucoes
padrao [] n = []
padrao ([]:t) n = padrao t (n+1)
padrao ((x:xs):t) n = saberSobe_Desce_Anda x n: padrao (xs:t) n

-- |  Funções que agrupam as peças iguais seguidas
agrupa :: (Eq a)=>[a]->[[a]]
agrupa [] = []
agrupa x = pinta (head x) (repeticoes x (head x)):(agrupa (elimina x (head x)) )

pinta :: (Eq a)=>a->Int->[a]
pinta c 0 = []
pinta c x = c:(pinta c (x-1))

elimina :: (Eq a)=>[a]->a->[a]
elimina [] _ = []
elimina (x:xs) y | x == y = (elimina xs y)
                 | otherwise = x:xs

repeticoes :: (Eq a)=>[a]->a->Int
repeticoes [] _ = 0
repeticoes (x:xs) y | x == y = 1 + repeticoes xs y
                    | otherwise = 0



-- | Função que vai agrupar Intruções.
criarRepetidos :: Instrucoes -> [Instrucoes]
criarRepetidos l = agrupa l

-- | Função para os padrões Horizontais
padraoHorizontal :: [Instrucoes] -> Instrucoes
padraoHorizontal [] = []
padraoHorizontal ([]:t) = padraoHorizontal t
padraoHorizontal ((x:xs):t) = if (length (x:xs) == 1) then x: padraoHorizontal t
                          else (Repete (length (x:xs)) [x]) : padraoHorizontal t




-- | Desconstrói um 'Mapa' numa sequência de 'Instrucoes'.
--
-- __NB:__ Uma solução correcta deve retornar uma sequência de 'Instrucoes' tal que, para qualquer mapa válido 'm', executar as instruções '(desconstroi m)' produza o mesmo mapa 'm'.
--
-- __NB:__ Uma boa solução deve representar o 'Mapa' dado no mínimo número de 'Instrucoes', de acordo com a função 'tamanhoInstrucoes'.
desconstroi :: Mapa -> Instrucoes
desconstroi m = padraoHorizontal(criarRepetidos(padrao (tiraPrimeira m) 0))



