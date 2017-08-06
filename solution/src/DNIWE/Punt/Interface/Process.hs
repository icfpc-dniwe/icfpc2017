module DNIWE.Punt.Interface.Process where

import Data.Maybe
import Control.DeepSeq
import Data.Default.Class

import DNIWE.Punt.Interface.Types
import DNIWE.Punt.Interface.Protocol
import DNIWE.Punt.Solver.Types
import DNIWE.Punt.Solver.Game
import DNIWE.Punt.Solver.Stupid

initializeState :: SetupRequest -> (GameData, SetupResponse)
initializeState setup = (game, game `deepseq` setupResp)
  where board = boardFromMap $ srMap setup
        Settings {..} = fromMaybe def $ srSettings setup
        myId = srPunter setup

        preGame = gameData board [] myId (srPunters setup)
        futures
          | settingsFutures = take 1 $ mapMaybe (maybeFuture preGame) $ stupidGameTree (3 * srPunters setup)preGame (initialState game)
          | otherwise = []
        setupResp = SetupResponse { srReady = myId
                                  , srFutures = map (uncurry PFuture) futures
                                  }
        game = gameData board (map (uncurry Future) futures) myId (srPunters setup)
