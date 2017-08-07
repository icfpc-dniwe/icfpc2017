{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}


module DNIWE.Punt.Interface.Offline where

import System.IO
import Control.Applicative
import GHC.Generics (Generic)
import qualified Data.HashMap.Lazy as HM
import Data.Serialize (Serialize(..))
import qualified Data.Serialize as Cer
import Data.Aeson (FromJSON(..), ToJSON(..), withObject, (.:))
import qualified Data.Aeson as JSON
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Binary
import Data.Conduit.Attoparsec

import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text.Encoding as TE

import DNIWE.Punt.Interface.Types
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

  
runOfflineClient :: Conduit JSON.Value IO JSON.Value -> IO ()
runOfflineClient conduit = do
  sourceHandle stdin $$ conduitParser messageParser =$= CL.map snd =$= conduit =$= CL.map encodeMessage =$= sinkHandle stdout
