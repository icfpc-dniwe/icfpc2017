module DNIWE.Punt.Interface.Online where

import Data.Monoid
import Data.Attoparsec.ByteString.Char8 as P
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Parser as JSONP
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Network
import Data.Conduit.Attoparsec

runJSONClient :: ByteString -> Int -> Conduit JSON.Value IO JSON.Value -> IO ()
runJSONClient host port conduit = runTCPClient (clientSettings port host) $ \appData -> do
  appSource appData $$ conduitParser (P.decimal >> P.char ':' >> JSONP.json) =$= CL.map snd =$= conduit =$= CL.map encodeValue =$= appSink appData

encodeValue :: JSON.Value -> ByteString
encodeValue val = B.pack (show $ B.length encoded) <> ":" <> encoded
  where encoded = BL.toStrict $ JSON.encode val
