import Data.Maybe
import Data.Default.Class
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Aeson as JSON
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Data.Graph.Inductive.Graph

import DNIWE.Punt.Interface.Offline
import DNIWE.Punt.Interface.Protocol
import DNIWE.Punt.Solver.Types
import DNIWE.Punt.Solver.Game
import DNIWE.Punt.Solver.Stupid

data State = State { stData :: GameData
                   , stState :: GameState
                   , stMyId :: PunterId
                   , stDepth :: Int
                   }
           deriving (Show, Eq, Generic)

instance Serialize State where

solver :: Int -> GameData -> GameState -> [Edge]
solver depth game state = map (actionEdge . fst) $ treeActions $ snd $ stupidGameTree depth game state

process :: OfflineRequest State -> Maybe (OfflineResponse State)
process (SetupOReq setup) =
  Just $ SetupORes $ StatefulMessage { smState = State {..}
                                     , smMessage = setupResp
                                     }

  where board = boardFromMap $ srMap setup
        Settings {..} = fromMaybe def $ srSettings setup
        stMyId = srPunter setup

        stDepth = 2 * srPunters setup

        preGame = gameData board [] stMyId (srPunters setup)
        futures
          | settingsFutures = take 1 $ mapMaybe (maybeFuture preGame) $ solver stDepth preGame (initialState preGame)
          | otherwise = []
        setupResp = SetupResponse { srReady = stMyId
                                  , srFutures = map (uncurry PFuture) futures
                                  }
        stData = gameData board (map (uncurry Future) futures) stMyId (srPunters setup)
        stState = initialState stData

process (GameOReq (StatefulMessage {..})) =
  Just $ GameORes $ StatefulMessage { smState = smState { stState = newState }
                                    , smMessage = move
                                    }

  where myId = stMyId smState
        playerFromId pid
          | pid < myId = Before pid
          | pid == myId = Us
          | otherwise = After pid

        applyMove' (Pass _) state = state
        applyMove' (Claim {..}) state = applyMove (playerFromId claimPunter) (claimSource, claimTarget) state

        newState = foldr applyMove' (stState smState) (movesMoves $ grMove smMessage)

        move = case solver (stDepth smState) (stData smState) newState of
                 (a, b):_ -> Claim { claimPunter = myId, claimSource = a, claimTarget = b }
                 [] -> Pass { passPunter = myId }

process (StopOReq _) = Nothing

main :: IO ()
main = do
  req' <- JSON.eitherDecode <$> BL.getContents
  case req' of
    Left e -> fail e
    Right (process -> Just resp) -> BL.putStrLn $ JSON.encode resp
    _ -> return ()
