{-# LANGUAGE DeriveGeneric #-}



module DNIWE.Punt.Protocol where
 -- export all

import Data.Aeson ()
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Data.Text as T

data Message = Message {
    msgNum  :: Int
  , msgData :: Text -- TODO parse?
  } deriving (Show, Eq, Generic)





