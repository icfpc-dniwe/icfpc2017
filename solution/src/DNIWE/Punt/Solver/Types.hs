module DNIWE.Punt.Solver.Types where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

type SiteId = Int
type PunterId = Int

data Player = Us | Other !PunterId
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
                  deriving (Show)

data EdgeContext = EdgeContext { edgeTaken :: Maybe Player
                               }
                 deriving (Show, Eq)

type Board = Gr () EdgeContext

data Game = Game { gameBoard :: Board
                 , gameMines :: Mines
                 , gameFutures :: Futures
                 , gameScoring :: MineScores
                 , gamePlayer :: Player
                 }
          deriving (Show)

data Action = Action { actionEdge :: Edge
                     , actionScore :: Double
                     , actionFeatures :: [Double]
                     }
            deriving (Show)

data GameTree = GameTree { treeState :: Game
                         , treeActions :: [(Action, GameTree)]
                         }
              deriving (Show)
