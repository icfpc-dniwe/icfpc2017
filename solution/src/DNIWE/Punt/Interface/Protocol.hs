{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}


module DNIWE.Punt.Interface.Protocol where
 -- export all

import Control.Monad (liftM2)

import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), object, (.=), (.:), (.:?))
import Data.Aeson.Types (typeMismatch)

import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Data.Text as T

{- TODO

  * issues

  * roadmap
    - [ ] message

    - [ ] online mode
      - [X] handshake
      - [ ] setup
      - [ ] gameplay
      - [ ] scoring

    - [ ] offline mode


-}


data Message = Message {
    msgNum  :: Int
  , msgData :: Text -- TODO parse?
  } deriving (Show, Eq, Generic)


-- - P -> S: {"me" : name}
-- - S -> P: {"you" : name}
data Handshake
  = HandshakeRequest  { hrName :: Text }
  | HandshakeResponse { hrName :: Text }
  deriving (Show, Eq)


instance ToJSON Handshake where
  toJSON (HandshakeRequest {..})  = object ["me".= hrName]
  toJSON (HandshakeResponse {..}) = object ["you" .= hrName]

instance FromJSON Handshake where
  parseJSON (Object v) = liftM2 (,) (v .:? "me") (v .:? "you") >>= \case
    (Just name, Nothing) -> return $ HandshakeRequest name
    (Nothing, Just name) -> return $ HandshakeResponse name
    _                    -> fail "Handshake :: unexpected structure"

  parseJSON invalid    = typeMismatch "Handshake" invalid
