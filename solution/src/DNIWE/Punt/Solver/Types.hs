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

data Future = Future { mine :: Mine
                     , target :: Node
                     }
            deriving (Show, Eq)
type Futures = Map Player [Future]

data StartingBoard = StartingBoard { sbBoard :: Gr () ()
                                   , sbMines :: Mines
                                   , sbFutures :: Futures
                                   }
                  deriving (Show)

data EdgeContext = EdgeContext { taken :: Maybe Player }
                 deriving (Show, Eq)

type Board = Gr () EdgeContext

data Game = Game { gameBoard :: Board
                 , gameMines :: Mines
                 , gameFutures :: Futures
                 , gameScoring :: MineScores
                 }
          deriving (Show)

data Action = Action { estimatedScore :: Double
                     , features :: [Double]
                     }
            deriving (Show)

data GameTree = GameTree { gameState :: Game
                         , actions :: [(Action, GameTree)]
                         }
              deriving (Show)
