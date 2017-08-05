module DNIWE.Punt.Solver.Types where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import GHC.Generics (Generic)
import Data.Serialize (Serialize(..))

type SiteId = Int
type PunterId = Int

instance (Serialize a, Serialize b) => Serialize (Gr a b) where
  put g = put (labNodes g) >> put (labEdges g)
  get = mkGraph <$> get <*> get

data Player = Us | After !PunterId | Before !PunterId
            deriving (Show, Eq, Ord, Generic)

instance Serialize Player where

type NodeScores = Map Node Int

type Mine = Node
type MineScores = Map Mine NodeScores
type Mines = Set Node

data Future = Future { futureMine :: Mine
                     , futureTarget :: Node
                     }
            deriving (Show, Eq, Generic)

instance Serialize Future where

type Futures = Map Player [Future]

data StartingBoard = StartingBoard { sbBoard :: Gr () ()
                                   , sbMines :: Mines
                                   }
                  deriving (Show, Eq, Generic)

instance Serialize StartingBoard where

data EdgeContext = EdgeContext { edgeTaken :: Maybe Player
                               }
                 deriving (Show, Eq, Ord, Generic)

instance Serialize EdgeContext where

type Board = Gr () EdgeContext

data GameData = GameData { gameStarting :: StartingBoard
                         , gameScoring :: MineScores
                         , gameFutures :: Futures
                         , gameBeforeN :: Int
                         , gameAfterN :: Int
                         }
              deriving (Show, Eq, Generic)

instance Serialize GameData where

data GameState = GameState { stateBoard :: Board
                           , statePlayer :: Player
                           }
               deriving (Show, Eq, Generic)

instance Serialize GameState where

data Action a = Action { actionEdge :: Edge
                       , actionScore :: a
                       }
              deriving (Show, Eq, Generic)

data GameTree a = GameTree { treeState :: GameState
                           , treeActions :: [(Action a, GameTree a)]
                           }
                deriving (Show, Eq, Generic)
