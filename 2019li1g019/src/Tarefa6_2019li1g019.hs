-- | Este módulo define funções comuns da Tarefa 6 do trabalho prático.
module Tarefa6_2019li1g019 where

import LI11920

{- | 
  <Relatório> : Tarefa 6
  -Introdução :
    A última tarefa deste projeto consiste na construção de um 'bot' que se desloque ao longo de um Mapa recebendo o identificador do Jogador,
  o 'Estado' para o qual o ro'bot' deve tomar uma decisão e uma possível 'Jogada' a efetuar pelo 'bot'.

  -Objetivos: 
    Para começar esta tarefa fizemos uma função que testa se o jogador está no chão ou no Ar, pois o mesmo só pode acelarar, se se encontrar no chão.
    Seguidamente, decidimos que seria necessário ter uma função que obtém os atritos já dados pelo enunciado.  Como queremos saber o atrito da peça 
  onde o jogador se encontra conseguimos fazê-lo encontrando a posição do jogador com o seu índice.
    Com isto conseguimos descobrir o atrito da peça onde o jogador está, da peça da pista que o antecede e da peça da pista que o sucede.
    Consegumos ainda comparar esses 3 atritos já calculados e caso o atrito da peça da pista acima for menor que os outros dois, então ele move-se
  para a pista de cima, caso o atrito da peça da pista de baixo for menor que os outros dois, então ele move-se para a pista de baixo, caso contrário,
  continua a deslocar-se na sua pista.

  
  -Discussão e conclusão
   Concluindo, acho que foi uma tarefa cativante e desafiante, pois nunca tinhamos desenvolvido um ro'bot' e conseguimos visualizar que um jogador
  poderia vir a ter num jogo feito por nós.
    Apesar da função que compara os atritos e faz com que o bot se movimente para cima, baixo e acelere esteja correta, pensamos que o ro'bot' não executa
  executa esses movimentos, logo as nossas dificuldades passaram por colocar o 'bot' a trocar de pista consoante o atrito de cada peça.
-}
    
-- | Vê se o jogador está no chão.
chaoTF :: Jogador -> Bool
chaoTF (Jogador _ _ _ _ (Chao _)) = True
chaoTF _ = False

-- | Observa se o jogador acelera.
chaoTrue :: Jogador -> Bool
chaoTrue (Jogador _ _ _ _ (Chao True)) = True
chaoTrue _ = False

-- | Recebe um jogador e retorna uma Maybe Jogada.
movimenta :: Jogador -> Maybe Jogada
movimenta j | (chaoTF j && chaoTrue j == False) = (Just Acelera) 
            |otherwise = Nothing 


-- | Função que obtém os atritos.
verAtrito :: Peca -> Double
verAtrito (Recta p _) | p == Relva = 0.75 
                      | p == Terra = 0.25
                      | p == Lama = 1.50
                      | p == Boost = -0.5
                      | p == Cola = 3.00 

verAtrito (Rampa p _ _) | p == Relva = 0.75 
                        | p == Terra = 0.25
                        | p == Lama = 1.50
                        | p == Boost = -0.5
                        | p == Cola = 3.00 

-- | Sabe a posição onde o jogador está com um índice.
getPosicao :: Int -> [a] -> a
getPosicao 0 (h:t) = h
getPosicao n (h:t) = getPosicao (n-1) t

-- | Calcula o atrito da peça onde o jogador se encontra.
atritoJogador :: Mapa -> Jogador -> Double
atritoJogador m (Jogador p d _ _ _) =  verAtrito (getPosicao (floor d)  (getPosicao p m))

-- | Calcula o atrito da peça na pista que antecede o jogador.
atritoCima :: Mapa -> Jogador -> Double
atritoCima m (Jogador p d _ _ _) |p>0 = verAtrito (getPosicao (floor d)  (getPosicao (p+1) m))
                                    |otherwise = verAtrito (getPosicao (floor d)  (getPosicao p m))

-- | Calcula o atrito da peça na pista que sucede o jogador.
atritoBaixo :: Mapa -> Jogador -> Double
atritoBaixo m (Jogador p d _ _ _) |(p< length m) = verAtrito (getPosicao (floor d)  (getPosicao (p-1) m))
                                  |otherwise = verAtrito (getPosicao (floor d)  (getPosicao p m))

-- | Recebendo um jogador e os 3 atritos já calculados, compara-os para efetuar uma Jogada.

comparaAtrito :: Jogador -> Double -> Double -> Double -> Maybe Jogada
comparaAtrito j a c b |(c<a)&&(c<b) && (chaoTF j && chaoTrue j == False) = (Just (Movimenta C))
                    |(b<a)&&(b<c) && (chaoTF j && chaoTrue j == False) = (Just (Movimenta B))
                    |(a<b)&&(a<c) && (chaoTF j && chaoTrue j == False) = (Just Acelera) 
                    |otherwise = Nothing

-- | Recebendo um Int e uma lista de jogadores, retorna o jogador pretendido.
getJogador :: Int -> [Jogador] -> Jogador
getJogador n (h:t)|(n>0) = getJogador (n-1) t
                  |otherwise = h
                  
-- * Funções principais da Tarefa 6.

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot n e@(Estado m l) =  comparaAtrito (getJogador n l )(atritoJogador m ( getJogador n l)) (atritoCima m (getJogador n l)) (atritoBaixo m (getJogador n l)) 


