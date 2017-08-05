module DNIWE.Punt.Solver.Types where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

type SiteId = Int
type PunterId = Int

data Player = Us | After !PunterId | Before !PunterId
            deriving (Show, Eq, Ord)

type NodeScores = Map Node Int

type Mine = Node
type MineScores = Map Mine NodeScores
type Mines = Set Node

data Future = Future { futureMine :: Mine
                     , futureTarget :: Node
                     }
            deriving (Show, Eq)
type Futures = Map Player [Future]

data StartingBoard = StartingBoard { sbBoard :: Gr () ()
                                   , sbMines :: Mines
                                   }
                  deriving (Show, Eq)

data EdgeContext = EdgeContext { edgeTaken :: Maybe Player
                               }
                 deriving (Show, Eq)

type Board = Gr () EdgeContext

data GameData = GameData { gameStarting :: StartingBoard
                         , gameScoring :: MineScores
                         , gameFutures :: Futures
                         , gameBeforeN :: Int
                         , gameAfterN :: Int
                         }
              deriving (Show)

data GameState = GameState { stateBoard :: Board
                           , statePlayer :: Player
                           }
               deriving (Show)

data Action a = Action { actionEdge :: Edge
                       , actionScore :: a
                       }
              deriving (Show)

data GameTree a = GameTree { treeState :: GameState
                           , treeActions :: [(Action a, GameTree a)]
                           }
                deriving (Show)
