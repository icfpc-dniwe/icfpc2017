import System.Environment
import Data.ByteString (ByteString)

import Control.Monad
import Control.Monad.Trans.Class (lift)
import qualified Data.Aeson as JSON (Value)
import Data.Aeson (FromJSON, Result(..), toJSON, fromJSON)
import Data.Conduit (Conduit, ConduitM, await, yield)

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
      myId = srPunter setup

      playerFromId pid
        | pid == myId = Us
        | otherwise = Other pid

      applyMove' (Pass _) game = game
      applyMove' (Claim {..}) game = applyMove (playerFromId claimPunter) (claimSource, claimTarget) game

  yield . toJSON $ SetupResponse { srReady = myId, srFutures = [] }

  let loop game prevMove = awaitJSON >>= \case
        GameReq greq@(GameplayRequest {..}) -> do
          lift . putStrLn $ "Got move: " ++ show greq
          unless (prevMove `elem` movesMoves grMove) $ fail "My move was rejected"

          let game' = foldr applyMove' game (movesMoves grMove)
              move = case stupidSolver game' of
                       Nothing -> Pass { passPunter = myId }
                       Just (a, b) -> Claim { claimPunter = myId, claimSource = a, claimTarget = b }

          lift . putStrLn $ "New game state: " ++ show game'
          lift . putStrLn $ "Sending move: " ++ show move
          yield $ toJSON move

          loop game' move

        StopReq stop@(StopRequest (Stop {..})) -> do
          lift . putStrLn $ "Got stop: " ++ show stop
          let finalGame = foldr applyMove' game (prevMove:stopMoves)
          lift $ forM_ stopScores $ \(Score {..}) -> do
            let player = playerFromId scorePunter
                score' = playerScore player finalGame
            putStrLn $ "Validating player " ++ show player ++ " score, server " ++ show scoreScore ++ ", us " ++ show score'
            unless (scoreScore == score') $ fail "Invalid score"

  loop (startingGame board) (Pass { passPunter = myId })
