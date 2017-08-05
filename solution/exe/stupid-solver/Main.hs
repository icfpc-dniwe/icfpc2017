import System.Environment
import Data.ByteString (ByteString)

import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Class (lift)
import qualified Data.Aeson as JSON (Value)
import Data.Aeson (FromJSON, Result(..), toJSON, fromJSON)
import Data.Conduit (Conduit, ConduitM, await, yield)
import Data.Default.Class
import Control.DeepSeq

import DNIWE.Punt.Interface.Online
import DNIWE.Punt.Interface.Protocol
import DNIWE.Punt.Solver.Types
import DNIWE.Punt.Solver.Game
import DNIWE.Punt.Solver.Score (playerScore)
import DNIWE.Punt.Solver.Stupid


gameServer :: ByteString
gameServer = "punter.inf.ed.ac.uk"

main :: IO ()
main = do
  [portString] <- getArgs
  let port = read portString
  runJSONClient gameServer port playGame


awaitJSON :: (Monad m, FromJSON a) => ConduitM JSON.Value b m a
awaitJSON = await >>= \case
  Nothing -> fail "Session has been finished prematurely"
  Just v  -> case (fromJSON v) of
    Error e   -> fail e
    Success x -> return x


playGame :: Conduit JSON.Value IO JSON.Value
playGame = do
  lift . putStrLn $ "Starting session"
  yield . toJSON $ HandshakeRequest { hrMe = "DNIWE :: a" }
  lift . putStrLn $ "Handshake requested"
  (hs :: HandshakeResponse) <- awaitJSON
  lift . putStrLn $ "Got response: " ++ show hs
  setup <- awaitJSON
  lift . putStrLn $ "Got setup: " ++ show setup

  let board = boardFromMap $ srMap setup
      Settings {..} = fromMaybe def $ srSettings setup
      myId = srPunter setup

      applyMove' (Pass _) state = state
      applyMove' (Claim {..}) state = applyMove claimPunter (claimSource, claimTarget) state

      solver game state = map (actionEdge . fst) $ treeActions $ snd $ stupidGameTree (3 * srPunters setup) game state

  let preGame = gameData board [] myId (srPunters setup)
      futures
        | settingsFutures = take 1 $ mapMaybe (maybeFuture preGame) $ solver preGame (initialState game)
        | otherwise = []
      setupResp = SetupResponse { srReady = myId
                                , srFutures = map (uncurry PFuture) futures
                                }
      game = gameData board (map (uncurry Future) futures) myId (srPunters setup)

  lift . putStrLn $ "Sending setup response: " ++ show setupResp
  yield $ toJSON $ game `deepseq` setupResp

  let loop state prevMove = awaitJSON >>= \case
        GameReq greq@(GameplayRequest {..}) -> do
          lift . putStrLn $ "Got move: " ++ show greq
          unless (prevMove `elem` movesMoves grMove) $ fail "My move was rejected"

          let newState = foldr applyMove' state (movesMoves grMove)
              move = case solver game newState of
                       (a, b):_ -> Claim { claimPunter = myId, claimSource = a, claimTarget = b }
                       [] -> Pass { passPunter = myId }

          lift . putStrLn $ "New game state: " ++ show newState
          lift . putStrLn $ ""
          lift . putStrLn $ "Sending move: " ++ show move
          lift . putStrLn $ ""
          yield $ toJSON move

          loop newState move

        StopReq stop@(StopRequest (Stop {..})) -> do
          lift . putStrLn $ "Got stop: " ++ show stop
          let finalState = foldr applyMove' state (prevMove:stopMoves)
          lift $ forM_ stopScores $ \(Score {..}) -> do
            let score' = playerScore game $ finalState { statePlayer = scorePunter }
            putStrLn $ "Validating player " ++ show scorePunter ++ " score, server " ++ show scoreScore ++ ", us " ++ show score'
            unless (scoreScore == score' || (scorePunter /= myId && settingsFutures)) $ fail "Invalid score"

  loop (initialState game) (Pass { passPunter = myId })
