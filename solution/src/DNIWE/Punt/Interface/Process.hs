module DNIWE.Punt.Interface.Process where

import Data.Maybe
import Control.Monad
import Control.DeepSeq
import Data.Default.Class
import Data.Graph.Inductive.Graph

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

initializeState :: SetupRequest -> (GameData, SetupResponse)
initializeState setup = (game, game `deepseq` setupResp)
  where board = boardFromMap $ srMap setup
        settings@(GameSettings {..}) = fromMaybe def $ srSettings setup
        myId = srPunter setup

        preGame = gameData board [] myId (srPunters setup) settings
        futures
          | settingsFutures = take 1 $ mapMaybe (gameMoveToFutureEdge >=> maybeFuture preGame) $ stupidGameTree (3 * srPunters setup)preGame (initialState game)
          | otherwise = []
        setupResp = SetupResponse { srReady = myId
                                  , srFutures = map (uncurry PFuture) futures
                                  }
        game = gameData board (map (uncurry Future) futures) myId (srPunters setup) settings
