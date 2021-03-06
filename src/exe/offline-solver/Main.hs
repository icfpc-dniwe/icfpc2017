import Data.Maybe
import qualified Data.Aeson as JSON
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Data.Conduit (Conduit)
import Control.Monad.Random
import Data.Graph.Inductive.Graph as G

import DNIWE.Punt.Interface.Types
import DNIWE.Punt.Interface.Offline
import DNIWE.Punt.Interface.Process
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

main :: IO ()
main = runOfflineClient playGame

playGame :: Conduit JSON.Value IO JSON.Value
playGame = do
  yieldJSON $ HandshakeRequest { hrMe = "DNIWE :: a" }
  (_ :: HandshakeResponse) <- awaitJSON

  msg <- awaitJSON
  case msg of
    SetupOReq setup -> do
      let stMyId = srPunter setup
      let board = boardFromMap $ srMap setup
      let numNodes = G.noNodes $ sbBoard board
          stDepth
            | numNodes < 16 = 3 * srPunters setup
            | numNodes < 32 = srPunters setup
            | numNodes < 64 = 2
            | otherwise = 1

      (stData, setupResp) <- lift . evalRandIO $ initializeState stDepth setup

      let stState = initialState stData

      yieldJSON $ StatefulMessage { smState = State {..}
                                  , smMessage = setupResp
                                  }

    GameOReq (StatefulMessage {..}) -> do
      let myId = stMyId smState
          newState = foldr (applyMove (stData smState)) (stState smState) (movesMoves $ grMove smMessage)

          move = makeMove myId $ listToMaybe $ stupidGameTree (stDepth smState) (stData smState) newState
      
      yieldJSON $ StatefulMessage { smState = smState { stState = newState }
                                  , smMessage = move
                                  }

    StopOReq _ -> return ()
