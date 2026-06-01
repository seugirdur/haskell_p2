module Main where

import System.Random
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad (when, forM_)

-- ================================================================
-- TIPOS
-- ================================================================

data Estado
  = Ocioso
  | Atacando
  | Defendendo
  | Atordoado
  | Morto
  deriving (Show, Eq, Ord)

data Entidade = Entidade
  { nome   :: String
  , hp     :: Int
  , estado :: Estado
  }

type TabelaTransicao = Map Estado [(Estado, Double)]

-- ================================================================
-- ASCII ART
-- ================================================================

heroiAscii :: [String]
heroiAscii =
  [ "         ___         "
  , "        [o_o]        "
  , "    [##]_|=|_>====== "
  , "        _|_|_        "
  , "       / | | \\       "
  , "      /_/   \\_\\      "
  ]

dragaoAscii :: [String]
dragaoAscii =
  [ "       \\(______     ______)/      "
  , "       /`.----.\\   /.----.`\\     "
  , "      } /      :} {:      \\ {    "
  , "     / {        } {        } \\   "
  , "     } }      ) } { (      { {   "
  , "    / {      /|\\}!{/|\\      } \\ "
  , "    } }     ( (.\"^\".) )     { {  "
  , "   / {       (d\\   /b)       } \\ "
  , "   } }       |\\~   ~/|       { {  "
  , "  / /        | )   ( |        \\ \\"
  , " { {        _)(,   ,)(_        } }"
  , "  } }      //  `" ++ "\";\"" ++ "`  \\\\      { {  "
  , " / /      //     (     \\\\      \\ \\ "
  , "{ {      {(     -=)     )}      } } "
  , " \\ \\     /)    -=(=-     (\\    / /  "
  , "  `\\\\  /'/    /-=|\\-\\    \\`\\  //'   "
  , "    `\\{  |   ( -===- )   |  }/'     "
  , "      `  _\\   \\-===-/   /_  '       "
  , "        (_(_(_)'-=-'(_)_)_)          "
  , "        `" ++ "\"`\"`\"" ++ "       " ++ "\"`\"`\"`"
  ]

exibirVS :: String -> String -> IO ()
exibirVS nomeH nomeD = do
  let colH     = 26
      padR n s = s ++ replicate (max 0 (n - length s)) ' '
      lH       = length heroiAscii   -- 6
      lD       = length dragaoAscii  -- 20
      topPad   = (lD - lH) `div` 2  -- 7: centers hero vertically
      heroiPad = replicate topPad "" ++ heroiAscii ++ repeat ""
      vsLine   = topPad + (lH `div` 2)  -- line where VS label appears
      tag i    = if i == vsLine then " VS " else "    "
  putStrLn ""
  mapM_ (\(i, (h, d)) -> putStrLn $ padR colH h ++ tag i ++ d)
        (zip [0..] $ take lD $ zip heroiPad dragaoAscii)
  putStrLn ""
  putStrLn $ padR colH ("  " ++ nomeH) ++ " VS   " ++ nomeD
  putStrLn ""

-- ================================================================
-- TABELAS DE TRANSICAO
-- ================================================================

tabelaHeroi :: TabelaTransicao
tabelaHeroi = Map.fromList
  [ (Ocioso,     [(Atacando, 0.5), (Defendendo, 0.3), (Ocioso,     0.2)])
  , (Atacando,   [(Ocioso,   0.4), (Atacando,   0.3), (Atordoado,  0.3)])
  , (Defendendo, [(Ocioso,   0.5), (Atacando,   0.2), (Defendendo, 0.3)])
  , (Atordoado,  [(Ocioso,   0.7), (Atordoado,  0.3)])
  , (Morto,      [(Morto,    1.0)])
  ]

tabelaDragao :: TabelaTransicao
tabelaDragao = Map.fromList
  [ (Ocioso,     [(Atacando, 0.6), (Defendendo, 0.2), (Ocioso,     0.2)])
  , (Atacando,   [(Atacando, 0.5), (Ocioso,     0.3), (Atordoado,  0.2)])
  , (Defendendo, [(Atacando, 0.5), (Ocioso,     0.3), (Defendendo, 0.2)])
  , (Atordoado,  [(Ocioso,   0.5), (Atordoado,  0.5)])
  , (Morto,      [(Morto,    1.0)])
  ]

-- ================================================================
-- EXIBICAO DA MATRIZ DE TRANSICAO
-- ================================================================

estadoAbrev :: Estado -> String
estadoAbrev Ocioso     = "Ocio"
estadoAbrev Atacando   = "Atk"
estadoAbrev Defendendo = "Def"
estadoAbrev Atordoado  = "Atrd"
estadoAbrev Morto      = "Mort"

mostrarMatriz :: String -> TabelaTransicao -> IO ()
mostrarMatriz titulo tabela = do
  let todos   = [Ocioso, Atacando, Defendendo, Atordoado, Morto]
      col     = 7
      padL n s = replicate (max 0 (n - length s)) ' ' ++ s
      padR n s = s ++ replicate (max 0 (n - length s)) ' '
      pct p   = show (round (p * 100) :: Int) ++ "%"
      linha   = replicate (6 + col * 5) '-'
  putStrLn $ "\n  Matriz de Transicao: " ++ titulo
  putStrLn linha
  putStrLn $ padR 6 "" ++ concatMap (\s -> padL col (estadoAbrev s)) todos
  putStrLn linha
  forM_ todos $ \s -> do
    let probs = Map.findWithDefault [] s tabela
        vals  = map (\t -> maybe 0.0 id (lookup t probs)) todos
    putStrLn $ padR 6 (estadoAbrev s) ++ concatMap (\p -> padL col (pct p)) vals
  putStrLn linha

-- ================================================================
-- NUCLEO DA CADEIA DE MARKOV: transicao de estado
-- ================================================================

proximoEstado :: TabelaTransicao -> Estado -> StdGen -> (Estado, StdGen)
proximoEstado tabela estadoAtual gen =
  let transicoes   = tabela Map.! estadoAtual
      (r, genNovo) = randomR (0.0 :: Double, 1.0) gen
      proximo      = amostrar transicoes r
  in (proximo, genNovo)

-- Percorre acumulando probabilidades ate encontrar o intervalo de `r`
amostrar :: [(Estado, Double)] -> Double -> Estado
amostrar [(s, _)] _        = s
amostrar ((s, p) : resto) r
  | r <= p                 = s
  | otherwise              = amostrar resto (r - p)
amostrar [] _              = Morto

-- ================================================================
-- CALCULO DE DANO
-- ================================================================

calcularDano :: Estado -> Estado -> Int -> StdGen -> (Int, StdGen)
calcularDano estadoAtk estadoDef atkBase gen =
  let fatorAtk = case estadoAtk of
        Atacando   -> 1.5
        Ocioso     -> 0.5
        _          -> 0.0
      fatorDef = case estadoDef of
        Defendendo -> 0.3
        Atordoado  -> 1.5
        _          -> 1.0
      danoBase         = round (fromIntegral atkBase * fatorAtk * fatorDef :: Double)
      (variacao, gen') = randomR (-2 :: Int, 2) gen
  in (max 0 (danoBase + variacao), gen')

-- ================================================================
-- SIMULACAO DE UM TURNO
-- ================================================================

simularTurno
  :: Int
  -> Entidade -> TabelaTransicao
  -> Entidade -> TabelaTransicao
  -> StdGen
  -> IO (Entidade, Entidade, StdGen)
simularTurno numTurno heroi tHeroi dragao tDragao gen = do
  putStrLn $ "\n--- Turno " ++ show numTurno ++ " ---"

  let (novoEstHeroi,  gen1) = proximoEstado tHeroi  (estado heroi)  gen
      (novoEstDragao, gen2) = proximoEstado tDragao (estado dragao) gen1

  putStrLn $ nome heroi  ++ ": " ++ show (estado heroi)  ++ " -> " ++ show novoEstHeroi
  putStrLn $ nome dragao ++ ": " ++ show (estado dragao) ++ " -> " ++ show novoEstDragao

  let (danoHeroi,  gen3) = calcularDano novoEstHeroi  novoEstDragao 10 gen2
      (danoDragao, gen4) = calcularDano novoEstDragao novoEstHeroi   8 gen3

  when (danoHeroi  > 0) $ putStrLn $ nome heroi  ++ " causa " ++ show danoHeroi  ++ " de dano!"
  when (danoDragao > 0) $ putStrLn $ nome dragao ++ " causa " ++ show danoDragao ++ " de dano!"

  let novoHpHeroi  = hp heroi  - danoDragao
      novoHpDragao = hp dragao - danoHeroi

  let estFinalHeroi  = if novoHpHeroi  <= 0 then Morto else novoEstHeroi
      estFinalDragao = if novoHpDragao <= 0 then Morto else novoEstDragao

  let heroiFinal  = Entidade (nome heroi)  (max 0 novoHpHeroi)  estFinalHeroi
      dragaoFinal = Entidade (nome dragao) (max 0 novoHpDragao) estFinalDragao

  putStrLn $ "  HP " ++ nome heroiFinal  ++ ": " ++ show (hp heroiFinal)
  putStrLn $ "  HP " ++ nome dragaoFinal ++ ": " ++ show (hp dragaoFinal)

  return (heroiFinal, dragaoFinal, gen4)

-- ================================================================
-- LOOP DA BATALHA
-- ================================================================

batalha
  :: Int
  -> Entidade -> TabelaTransicao
  -> Entidade -> TabelaTransicao
  -> StdGen
  -> IO ()
batalha _ heroi _ dragao _ _
  | estado heroi  == Morto = putStrLn $ "\n=== " ++ nome dragao ++ " venceu! Game Over. ==="
  | estado dragao == Morto = putStrLn $ "\n=== " ++ nome heroi  ++ " venceu! Parabens! ==="
batalha turnoAtual heroi tHeroi dragao tDragao gen = do
  (novoHeroi, novoDragao, novoGen) <-
    simularTurno turnoAtual heroi tHeroi dragao tDragao gen
  batalha (turnoAtual + 1) novoHeroi tHeroi novoDragao tDragao novoGen

-- ================================================================
-- MAIN
-- ================================================================

main :: IO ()
main = do
  gen <- newStdGen

  let heroi  = Entidade { nome = "Heroi",  hp = 50, estado = Ocioso }
      dragao = Entidade { nome = "Dragao", hp = 60, estado = Ocioso }

  putStrLn "============================================"
  putStrLn "    BATALHA RPG - CADEIAS DE MARKOV"
  putStrLn "============================================"

  exibirVS (nome heroi) (nome dragao)

  putStrLn $ "HP " ++ nome heroi  ++ ": " ++ show (hp heroi)
  putStrLn $ "HP " ++ nome dragao ++ ": " ++ show (hp dragao)

  mostrarMatriz "Heroi"  tabelaHeroi
  mostrarMatriz "Dragao" tabelaDragao

  putStrLn "\n============================================"
  putStrLn "           INICIO DA BATALHA"
  putStrLn "============================================"

  batalha 1 heroi tabelaHeroi dragao tabelaDragao gen
