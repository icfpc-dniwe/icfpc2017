{-# LANGUAGE LambdaCase #-}

import System.Environment
import Data.ByteString (ByteString)

import Control.Monad.Trans.Class (lift)
import qualified Data.Aeson as JSON (Value)
import Data.Aeson (FromJSON, Result(..), toJSON, fromJSON)
import Data.Conduit (Conduit, ConduitM, await, yield)
import Data.Proxy (Proxy(..))
import DNIWE.Punt.Interface.Online
import DNIWE.Punt.Interface.Protocol (HandshakeRequest(..), HandshakeResponse(..), SetupRequest(..), boardFromMap)
import Control.Concurrent.MVar(MVar, newEmptyMVar, putMVar, tryTakeMVar)
import DNIWE.Punt.Solver.Types (Board)


gameServer :: ByteString
gameServer = "punter.inf.ed.ac.uk"

main :: IO ()
main = do
  [portString] <- getArgs
  let port = read portString
  mvar <- newEmptyMVar
  runJSONClient gameServer port (getMap mvar)
  tryTakeMVar mvar >>= maybe
    (putStrLn "nothing here")
    (putStrLn . show)


await' :: (Monad m, FromJSON a) => Proxy a -> ConduitM JSON.Value b m a
await' _ = await >>= \case
  Nothing -> error "session has been finished prematurely"
  Just v  -> case (fromJSON v) of
    Error e   -> error e
    Success x -> return x


getMap :: MVar Board -> Conduit JSON.Value IO JSON.Value
getMap mvar = do
  lift . putStrLn $ "getMap started"
  yield . toJSON $ HandshakeRequest { hrMe = "DNIWE :: a" }
  await' (Proxy :: Proxy HandshakeResponse) >>= lift . putStrLn . show
  await' (Proxy :: Proxy SetupRequest) >>= lift . putMVar mvar . boardFromMap . srMap
  lift . putStrLn $ "getMap finished"
