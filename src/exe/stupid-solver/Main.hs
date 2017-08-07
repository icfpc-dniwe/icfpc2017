import System.Environment
import Data.ByteString (ByteString)

import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Class (lift)
import qualified Data.Aeson as JSON (Value)
import Data.Conduit (Conduit)
import Control.Monad.Random
import Data.Graph.Inductive.Graph as G
import qualified Data.IntMap.Strict as IM

import DNIWE.Punt.Interface.Online
import DNIWE.Punt.Interface.Protocol
import DNIWE.Punt.Interface.Types
import DNIWE.Punt.Interface.Process
import DNIWE.Punt.Solver.Score
import DNIWE.Punt.Solver.Types
import DNIWE.Punt.Solver.Game
import DNIWE.Punt.Solver.Stupid


gameServer :: ByteString
gameServer = "punter.inf.ed.ac.uk"

main :: IO ()
main = do
  [portString] <- getArgs
  let port = read portString
  runJSONClient gameServer port playGame


playGame :: Conduit JSON.Value IO JSON.Value
playGame = do
  lift . putStrLn $ "Starting session"
  yieldJSON $ HandshakeRequest { hrMe = "DNIWE :: a" }
  lift . putStrLn $ "Handshake requested"
  (hs :: HandshakeResponse) <- awaitJSON
  lift . putStrLn $ "Got response: " ++ show hs
  setup <- awaitJSON
  lift . putStrLn $ "Got setup: " ++ show setup

  (game, setupResp) <- lift . evalRandIO $ initializeState setup

  let myId = srPunter setup
      GameSettings {..} = gameSettings game

  lift . putStrLn $ "Game data: " ++ show game
  lift . putStrLn $ "Sending setup response: " ++ show setupResp
  yieldJSON setupResp

  let loop state prevMove = awaitJSON >>= \case
        GameReq greq@(GameplayRequest {..}) -> do
          lift . putStrLn $ "Got move: " ++ show greq
          unless (prevMove `elem` movesMoves grMove) $ fail "My move was rejected"

          let newState = foldr (applyMove game) state (movesMoves grMove)
              numNodes = G.noNodes $ sbBoard $ gameStarting game
              stupidDepth
                | numNodes < 16 = 3 * gamePlayersN game
                | numNodes < 32 = gamePlayersN game
                | otherwise = 1
              move = makeMove myId $ listToMaybe $ stupidGameTree stupidDepth game newState

          lift . putStrLn $ "New game state: " ++ show newState
          lift . putStrLn $ ""
          lift . putStrLn $ "Sending move: " ++ show move
          lift . putStrLn $ ""
          yieldJSON move

          loop newState move

        StopReq stop@(StopRequest (Stop {..})) -> do
          lift . putStrLn $ "Got stop: " ++ show stop
          let finalState = foldr (applyMove game) state (prevMove:stopMoves)
          lift . putStrLn $ "Final game state: " ++ show finalState
          lift $ forM_ stopScores $ \(ScoreResponse {..}) -> do
            let scoreFast = stateScores finalState IM.! scorePunter
            let scoreSlow = playerScore game $ finalState { statePlayer = scorePunter }

            putStrLn $ "Validating player " ++ show scorePunter ++ " score, server " ++ show scoreScore ++ ", us fast " ++ show scoreFast ++ ", us slow " ++ show scoreSlow
            -- unless (scoreScore == score' || (scorePunter /= myId && settingsFutures)) $ fail "Invalid score"

  loop (initialState game) (Pass { passPunter = myId })
