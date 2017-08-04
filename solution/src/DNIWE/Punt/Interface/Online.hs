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
  appSource appData $$ conduitParser msgParser =$= CL.map snd =$= conduit =$= CL.map encodeValue =$= appSink appData

msgParser :: P.Parser JSON.Value
msgParser = do
  size <- P.decimal
  _ <- P.char ':'
  msg <- P.take size
  case P.parseOnly JSONP.json msg of
    Left e -> fail e
    Right r -> return r

encodeValue :: JSON.Value -> ByteString
encodeValue val = B.pack (show $ B.length encoded) <> ":" <> encoded
  where encoded = BL.toStrict $ JSON.encode val
