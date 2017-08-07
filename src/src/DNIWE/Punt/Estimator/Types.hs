module DNIWE.Punt.Estimator.Types where
 -- export all

import Data.Graph.Inductive.Graph
import Data.Aeson (ToJSON(..), FromJSON(..), genericParseJSON, genericToJSON, genericToEncoding)
import Data.Aeson.Types (Options(..), SumEncoding(..), defaultOptions)
import GHC.Generics (Generic)

import DNIWE.Punt.Interface.Utility


jsonOptions :: Options
jsonOptions = defaultOptions { constructorTagModifier = snakeCase . dropSuffix . dropSuffix
                             , fieldLabelModifier = snakeCase . dropPrefix
                             , sumEncoding = TaggedObject { tagFieldName = "action", contentsFieldName = error "jsonOptions: contentsFieldName" }
                             }


data EdgeProbability = EdgeProbability { candidateSrc :: Node
                                       , candidateDst :: Node
                                       , candidateProbability :: [Double]
                                       }
                   deriving (Show, Eq, Generic)

instance ToJSON EdgeProbability where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON EdgeProbability where
  parseJSON = genericParseJSON jsonOptions


data EstimatorRequest = SettingsEstRequest
                      | IncidenceEstRequest
                      | PutProbabilitiesEstRequest { estimatorValues :: [EdgeProbability] }
                      | PutActionEstRequest { estimatorSource :: Node, estimatorTarget :: Node }
                      | IsFinishedEstRequest
                      deriving (Show, Eq, Generic)

instance ToJSON EstimatorRequest where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON EstimatorRequest where
  parseJSON = genericParseJSON jsonOptions


data IncidenceEdge = IncidenceEdge { incidenceSrc :: Node
                                   , incidenceDst :: Node
                                   , incidenceFeatures :: [Double]
                                   , incidenceValid :: Bool
                                   }
                   deriving (Show, Eq, Generic)

instance ToJSON IncidenceEdge where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON IncidenceEdge where
  parseJSON = genericParseJSON jsonOptions


data EstimatorResponse = SettingsEstResponse { estimatorFeatureCount :: Int, estimatorReturnProb :: Bool }
                       | IncidenceEstResponse { estimatorEdges :: [IncidenceEdge] }
                       | PutProbabilitiesEstResponse { estimatorReward :: Double }
                       | PutActionEstResponse { estimatorReward :: Double }
                       | IsFinishedEstResponse { estimatorIsFinished :: Bool }
                       deriving (Show, Eq, Generic)

instance ToJSON EstimatorResponse where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON EstimatorResponse where
  parseJSON = genericParseJSON jsonOptions
