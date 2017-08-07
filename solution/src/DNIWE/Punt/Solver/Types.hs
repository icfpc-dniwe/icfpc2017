{-# OPTIONS_GHC -fno-warn-orphans #-}

module DNIWE.Punt.Solver.Types where

import Data.IntSet (IntSet)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import GHC.Generics (Generic)
import Data.Serialize (Serialize(..))
import Control.DeepSeq (NFData(..))
import Data.Hashable

type SiteId = Int
type PunterId = Int

type Score = Int

instance (Serialize a, Serialize b) => Serialize (Gr a b) where
  put g = put (labNodes g) >> put (labEdges g)
  get = mkGraph <$> get <*> get

instance (Hashable k, Hashable a) => Hashable (Map k a) where
  hashWithSalt s m = hashWithSalt s (M.toAscList m)

instance (Hashable a) => Hashable (IntMap a) where
  hashWithSalt s m = hashWithSalt s (IM.toAscList m)

type NodeScores = IntMap Int

type Mine = Node
type MineScores = IntMap NodeScores
type Mines = IntSet

data Future = Future { futureMine :: !Mine
                     , futureTarget :: !Node
                     }
            deriving (Show, Eq, Generic)

instance Serialize Future where
instance NFData Future where

type Futures = IntMap [Future]

type Board = Gr () ()

data StartingBoard = StartingBoard { sbBoard :: !Board
                                   , sbMines :: !Mines
                                   }
                  deriving (Show, Eq, Generic)

instance Serialize StartingBoard where
instance NFData StartingBoard where

type NearestEdges = IntMap (Set Edge)

data GameSettings = GameSettings { settingsFutures :: !Bool
                                 , settingsSplurges :: !Bool
                                 , settingsOptions :: !Bool
                                 }
                  deriving (Show, Eq, Generic)

instance Serialize GameSettings where
instance NFData GameSettings where

data GameData = GameData { gameStarting :: !StartingBoard
                         , gameScoring :: !MineScores
                         , gameFutures :: !Futures
                         , gameSettings :: !GameSettings
                         , gameMyId :: !PunterId
                         , gamePlayersN :: !Int
                         , gameEdgesNearMines :: !NearestEdges
                         }
              deriving (Show, Eq, Generic)

instance Serialize GameData where
instance NFData GameData where

data GameState = GameState { stateTaken :: !(Map Edge PunterId)
                           , statePlayer :: !PunterId
                           , stateRemainingOptions :: !(IntMap Int)
                           }
               deriving (Show, Eq, Generic)

instance Serialize GameState where
instance Hashable GameState where

data Action a = Action { actionEdge :: !Edge
                       , actionScore :: !a
                       }
              deriving (Show, Eq, Generic)

data GameTree a = GameTree { treeState :: !GameState
                           , treeActions :: [(Action a, GameTree a)]
                           }
                deriving (Show, Eq, Generic)

data GameMove = MoveClaim Edge
              | MoveOption Edge
              | MovePass
              | MoveSplurge [Edge]
              deriving (Show, Eq)
