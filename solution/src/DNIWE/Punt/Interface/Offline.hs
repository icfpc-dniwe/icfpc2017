{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}


module DNIWE.Punt.Interface.Offline where

import Control.Applicative
import GHC.Generics (Generic)
import qualified Data.HashMap.Lazy as HM
import Data.Serialize (Serialize(..))
import qualified Data.Serialize as Cer
import Data.Aeson (FromJSON(..), ToJSON(..), withObject, (.:))
import qualified Data.Aeson as JSON

import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text.Encoding as TE

import DNIWE.Punt.Interface.Protocol


data StatefulMessage state msg = StatefulMessage { smState :: state
                                                 , smMessage :: msg
                                                 }
                               deriving (Show, Eq, Generic)

instance (ToJSON msg, Serialize state) => ToJSON (StatefulMessage state msg) where
  toJSON smsg = JSON.Object $ HM.insert "state" (JSON.String state) obj
    where (JSON.Object obj) = toJSON $ smMessage smsg
          state = TE.decodeUtf8 $ Base64.encode $ Cer.encode $ smState smsg

instance (FromJSON msg, Serialize state) => FromJSON (StatefulMessage state msg) where
  parseJSON = withObject "StatefulMessage" $ \obj -> do
    smMessage <- parseJSON $ JSON.Object obj
    state <- obj .: "state"
    case Base64.decode $ TE.encodeUtf8 state of
      Left e -> fail e
      Right bstate -> case Cer.decode bstate of
        Left e -> fail e
        Right smState -> return StatefulMessage {..}


data OfflineRequest state = SetupOReq SetupRequest
                          | StopOReq (StatefulMessage state StopRequest)
                          | GameOReq (StatefulMessage state GameplayRequest)
                          deriving (Show, Eq)

instance (Serialize state) => FromJSON (OfflineRequest state) where
  parseJSON p = (SetupOReq <$> parseJSON p) <|> (StopOReq <$> parseJSON p) <|> (GameOReq <$> parseJSON p)

instance (Serialize state) => ToJSON (OfflineRequest state) where
  toJSON (SetupOReq setup) = toJSON setup
  toJSON (StopOReq stop) = toJSON stop
  toJSON (GameOReq game) = toJSON game

  
data OfflineResponse state = SetupORes (StatefulMessage state SetupResponse)
                           | GameORes (StatefulMessage state GameplayResponse)
                           deriving (Show, Eq)

instance (Serialize state) => FromJSON (OfflineResponse state) where
  parseJSON p = (SetupORes <$> parseJSON p) <|> (GameORes <$> parseJSON p)

instance (Serialize state) => ToJSON (OfflineResponse state) where
  toJSON (SetupORes setup) = toJSON setup
  toJSON (GameORes game) = toJSON game
