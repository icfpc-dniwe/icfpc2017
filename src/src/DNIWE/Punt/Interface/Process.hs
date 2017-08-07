module DNIWE.Punt.Interface.Process where

import Data.Maybe
import Control.Monad
import Control.DeepSeq
import Data.Default.Class
import Data.Graph.Inductive.Graph
import Control.Monad.Random.Class

import DNIWE.Punt.Interface.Types
import DNIWE.Punt.Interface.Protocol
import DNIWE.Punt.Solver.Types
import DNIWE.Punt.Solver.Game
import DNIWE.Punt.Solver.Stupid

gameMoveToFutureEdge :: GameMove -> Maybe Edge
gameMoveToFutureEdge (MoveClaim e) = Just e
gameMoveToFutureEdge (MoveOption e) = Just e
gameMoveToFutureEdge (MoveSplurge es) = Just $ head es
gameMoveToFutureEdge (MovePass) = Nothing

extractSettings :: SettingsResponse -> GameSettings
extractSettings (SettingsResponse {..}) =
  GameSettings { settingsFutures = fromMaybe False setrespFutures
               , settingsSplurges = fromMaybe False setrespSplurges
               , settingsOptions = fromMaybe False setrespOptions
               }

initializeState :: MonadRandom m => Int -> SetupRequest -> m (GameData, SetupResponse)
initializeState depth setup = do
  let board = boardFromMap $ srMap setup
      settings@(GameSettings {..}) = extractSettings $ fromMaybe def $ srSettings setup
      myId = srPunter setup

  preGame <- gameData board [] myId (srPunters setup) settings

  let futures
        | settingsFutures = take 1 $ mapMaybe (gameMoveToFutureEdge >=> maybeFuture preGame) $ stupidGameTree depth preGame (initialState preGame)
        | otherwise = []
      setupResp = SetupResponse { srReady = myId
                                , srFutures = map (uncurry PFuture) futures
                                }
  game <- gameData board (map (uncurry Future) futures) myId (srPunters setup) settings

  return (game, game `deepseq` setupResp)
