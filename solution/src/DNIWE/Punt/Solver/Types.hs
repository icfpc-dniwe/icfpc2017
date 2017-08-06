{-# OPTIONS_GHC -fno-warn-orphans #-}

module DNIWE.Punt.Solver.Types where

import Data.IntMap.Strict (IntMap)
import Data.Set (Set)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import GHC.Generics (Generic)
import Data.Serialize (Serialize(..))
import Control.DeepSeq (NFData(..))

type SiteId = Int
type PunterId = Int

instance (Serialize a, Serialize b) => Serialize (Gr a b) where
  put g = put (labNodes g) >> put (labEdges g)
  get = mkGraph <$> get <*> get

type NodeScores = IntMap Int

type Mine = Node
type MineScores = IntMap NodeScores
type Mines = Set Node

data Future = Future { futureMine :: !Mine
                     , futureTarget :: !Node
                     }
            deriving (Show, Eq, Generic)

instance Serialize Future where
instance NFData Future where

type Futures = IntMap [Future]

data StartingBoard = StartingBoard { sbBoard :: !(Gr () ())
                                   , sbMines :: !Mines
                                   }
                  deriving (Show, Eq, Generic)

instance Serialize StartingBoard where
instance NFData StartingBoard where

data EdgeContext = EdgeContext { edgeTaken :: !(Maybe PunterId)
                               }
                 deriving (Show, Eq, Ord, Generic)

instance Serialize EdgeContext where

type Board = Gr () EdgeContext

data GameData = GameData { gameStarting :: !StartingBoard
                         , gameScoring :: !MineScores
                         , gameFutures :: !Futures
                         , gameMyId :: !PunterId
                         , gamePlayersN :: !Int
                         }
              deriving (Show, Eq, Generic)

instance Serialize GameData where
instance NFData GameData where

data GameState = GameState { stateBoard :: !Board
                           , statePlayer :: !PunterId
                           }
               deriving (Show, Eq, Generic)

instance Serialize GameState where

data Action a = Action { actionEdge :: !Edge
                       , actionScore :: !a
                       }
              deriving (Show, Eq, Generic)

data GameTree a = GameTree { treeState :: !GameState
                           , treeActions :: [(Action a, GameTree a)]
                           }
                deriving (Show, Eq, Generic)

data GameMove = MoveClaim Edge |
                MovePass |
                MoveSplurge [Edge]
              deriving (Show, Eq)