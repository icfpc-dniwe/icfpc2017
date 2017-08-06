module DNIWE.Punt.Interface.Online where

import Data.ByteString (ByteString)
import qualified Data.Aeson as JSON
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Network
import Data.Conduit.Attoparsec

import DNIWE.Punt.Interface.Protocol

runJSONClient :: ByteString -> Int -> Conduit JSON.Value IO JSON.Value -> IO ()
runJSONClient host port conduit = runTCPClient (clientSettings port host) $ \appData -> do
  appSource appData $$ conduitParser messageParser =$= CL.map snd =$= conduit =$= CL.map encodeMessage =$= appSink appData
