module DNIWE.Punt.Interface.Online where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Control.Monad.Trans.Control
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Parser as JSONP
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Network
import Data.Conduit.Attoparsec

runJSONClient :: ByteString -> Int -> Conduit JSON.Value IO JSON.Value -> IO ()
runJSONClient host port conduit = runTCPClient (clientSettings port host) $ \appData -> do
  appSource appData $$ conduitParser JSONP.json =$= CL.map snd =$= conduit =$= CL.map (BL.toStrict . JSON.encode) =$= appSink appData
