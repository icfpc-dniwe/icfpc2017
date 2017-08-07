module DNIWE.Punt.Estimator.Protocol where

import Control.Monad
import Control.DeepSeq
import Data.Maybe
import qualified Data.Aeson as JSON
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Map as M
import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict as IM
import Data.Conduit.Process
import Data.Conduit.Attoparsec
import System.Process as P
import System.Timeout
import Data.Graph.Inductive.Graph
import Control.Exception
import Control.Monad.Trans.Class
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue

import DNIWE.Punt.Estimator.Types
import DNIWE.Punt.Interface.Protocol
import DNIWE.Punt.Solver.Types
import DNIWE.Punt.Solver.Game

defaultFeatureCount :: Int
defaultFeatureCount = 4

gameIncidence :: GameData -> GameState -> [IncidenceEdge]
gameIncidence game state = map makeOne $ labEdges $ sbBoard $ gameStarting game
  where makeOne (from, to, _) = IncidenceEdge { incidenceSrc = from
                                              , incidenceDst = to
                                              , incidenceFeatures = [ fromIntegral scoreImprovement
                                                                    , boolAsDouble nodesAreMines
                                                                    , boolAsDouble takenByUs
                                                                    , boolAsDouble takenByThem
                                                                    ]
                                              , incidenceValid = isJust takenPlayer
                                              }

          where state' = performClaim game player (from, to) state
                takenPlayer = M.lookup (from, to) $ stateTaken state

                scoreImprovement = stateScores state' IM.! player - stateScores state IM.! player
                nodesAreMines = nodeIsMine from || nodeIsMine to
                takenByUs = takenPlayer == Just player
                takenByThem = isJust takenPlayer && not takenByUs 

        boolAsDouble True = 1
        boolAsDouble False = 0
          
        nodeIsMine n = n `IS.member` sbMines (gameStarting game)

        player = gameMyId game
  
runEstimatorClient :: FilePath -> (TMVar GameData -> TMQueue GameState -> TQueue Edge -> IO a) -> IO a
runEstimatorClient path comp = do
  gameDataVar <- newEmptyTMVarIO
  gameStateQueue <- newTMQueueIO
  syncVar <- newEmptyTMVarIO
  requestQueue <- newTQueueIO
  resultQueue <- newTQueueIO

  let processRequest SettingsEstRequest = return $ SettingsEstResponse { estimatorFeatureCount = defaultFeatureCount
                                                                       , estimatorReturnProb = False
                                                                       }
      processRequest IncidenceEstRequest = do
        void $ atomically $ tryPutTMVar syncVar ()
        (curGame, Just curState) <- atomically $ (,) <$> readTMVar gameDataVar <*> readTMQueue gameStateQueue
        return $ IncidenceEstResponse { estimatorEdges = gameIncidence curGame curState }
      processRequest (PutProbabilitiesEstRequest {..}) = fail "probabilities are not implemented"
      processRequest (PutActionEstRequest {..}) = do
        atomically $ writeTQueue resultQueue (estimatorSource, estimatorTarget)
        return $ PutActionEstResponse { estimatorReward = 0 }
      processRequest IsFinishedEstRequest = do
        isFinished <- atomically (isNothing <$> peekTMQueue gameStateQueue)
        return IsFinishedEstResponse { estimatorIsFinished = isFinished }

      processJSON = await >>= \case
        Nothing -> return ()
        Just (_, v) -> case JSON.fromJSON v of
          JSON.Error e   -> fail e
          JSON.Success x -> yield x >> processJSON
      
      stdinProducer = forever (lift (atomically $ readTQueue requestQueue) >>= yield) =$= CL.mapM processRequest =$= CL.map (encodeMessage . JSON.toJSON)
      stdoutConsumer = conduitParser messageParser =$= processJSON =$= CL.mapM_ (atomically . writeTQueue requestQueue)

  let conduitThread = sourceProcessWithStreams (P.proc path []) stdinProducer stdoutConsumer (return ())
  withAsync conduitThread $ \_ -> do
    void $ atomically $ takeTMVar syncVar
    comp gameDataVar gameStateQueue resultQueue

runMove :: Int -> (GameData -> GameState -> Edge) -> TMVar GameData -> TMQueue GameState -> TQueue Edge -> GameData -> GameState -> IO Edge
runMove timeoutVal solver gameDataVar gameStateQueue resultQueue game state = do
  let solverAsync = do
        let ret' = solver game state
        !ret <- evaluate $ force ret'
        return ret

  estimatorResult <- async $ do
    atomically $ do
      void $ tryPutTMVar gameDataVar game
      writeTMQueue gameStateQueue state
    atomically $ readTQueue resultQueue

  withAsync (solverAsync :: IO Edge) $ \solverResult -> do
    goodRet <- timeout timeoutVal $ wait solverResult
    case goodRet of
      Just ret -> return ret
      Nothing -> either id id <$> waitEither solverResult estimatorResult
