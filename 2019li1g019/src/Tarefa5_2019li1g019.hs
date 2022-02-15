-- | Este módulo define funções comuns da Tarefa 5 do trabalho prático.
module Main where


import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy
import LI11920


{- | 
      <Relatório> : Tarefa 5
      -Introdução : 
            Esta tarefa tem o intuito de nos fazer visualizar o cenário de todo o jogo programado e aprender a utilizar novas bibliotecas,
      que neste é o Gloss.

      -Objetivos: 
      O nosso foco, nesta parte do trabalho, foi conseguir que o jogo acabasse como pretendido. 
      O nosso pensamento passou por fazer um mapa a movimentar-se, começando por desenvolver a função "desenhaMapa" 
      que posteriormente é utilizada no "desenhaEstado".
        
      
      -Discussão e conclusão :
      Concluindo, esta tarefa foi realmente um pouco complicada, pois devido ao pouco conhecimento que tínhamos desta biblioteca, 
não conseguimos desenvolver esta tarefa por completo.
      Achámos que talvez deveréamos ter trabalhado um pouco mais nesta tarefa, devido a ser uma novidade e assim teríamos desenvolvido mais capacidades.
      
-}

-- | Dada uma lista, retorna os valores entre os índices dados.
listaEntre2Indices :: Int -> Int -> [a]-> [a]
listaEntre2Indices 0 f l = take (f+1) l
listaEntre2Indices i f (h:t) = listaEntre2Indices (i-1) (f-1) t
listaEntre2Indices _ _ _ = []

 
estadoInicial :: Estado
estadoInicial = undefined

-- | 
{-desenhaEstado :: Estado -> [Picture] -> [Picture]
desenhaEstado  = (Estado m((Jogador _ d _ _ _):_)) textures = desenhaMapa mt (-1000-d1) (600+d2) textures
                                                            where d1 = realToFrac d
                                                                  d2 = (realToFrac d)*0.6
-}
-- | Desenha um Mapa peça a peça.                                                            
{-desenhaMapa :: Mapa -> Float -> Float -> [Picture] -> [Picture]
desenhaMapa (h:t) x y textures = (desenhaPista h x y textures:desenhaMapa h (x+150) (y-90) textures
desenhaMapa _ _ _ _ = []
-}

desenhaPista :: Pista -> Float -> Float -> [Picture] -> Picture
desenhaPista = undefined

-- | Reage a um input.
reageEvento :: Event -> Estado -> Estado
reageEvento _ s = s

-- | Reage À passagem do tempo.
reageTempo :: Float -> Estado -> Estado
reageTempo n (Estado m (Jogador a b c d e):t) = (Estado m (Jogador a (b+2) c d e):t)

--Estado Gloss

type EstadoGloss = (Estado,[Picture])

-- | Da-nos o estado inicial do Gloss.
estadoGlossInicial :: Picture -> EstadoGloss
estadoGlossInicial eg = (estadoInicial, eg)

-- | 
desenhaEstadoGloss :: EstadoGloss -> Picture
desenhaEstadoGloss (e,li) = desenhaEstado e textures
                           where textures = listaEntre2Indices 0 5

-- | Reage a um evento do Gloss.
reageEventoGloss :: Event -> EstadoGloss -> EstadoGloss
reageEventoGloss ev (e,li) = (reageEvento ev e,li)

-- | Reage à medida do tempo.
reageTempoGloss :: Float -> EstadoGloss -> EstadoGloss
reageTempoGloss t (e,li) = (reageTempo t e,li)

-- | Frame Rate
fr:: Int
fr = 150

-- | Janela como o jogo é iniciado.
ds :: Display
ds = FullScreen

-- | Função principal da Tarefa 5.
--
-- __NB:__ Esta Tarefa é completamente livre. Deve utilizar a biblioteca <http://hackage.haskell.org/package/gloss gloss> para animar o jogo, e reutilizar __de forma completa__ as funções das tarefas anteriores.
{main :: IO ()
main = do (Just p1 <- loadJuicy "Terra.png"
p2 <- loadJuicy "Relva.png"
p3 <- loadJuicy "Lama.png"
p4 <- loadJuicy "Boost.png"
p5 <- loadJuicy "Cola.png"
	   play dm                   --janela onde corre
	   (greyN 0.5)               -- cor do fundo
       fr                        -- frame rate
       (estadoGlossInicial [p1,p2,p3,p4,p5])   -- estado inicial
       desenhaEstadoGloss        -- desenha o estado do jogo
       reageEventoGloss          -- reage a um evento
       reageTempoGloss )          -- reage ao passar do tempo
