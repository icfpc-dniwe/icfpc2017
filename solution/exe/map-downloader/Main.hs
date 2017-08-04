import System.Environment
import Data.ByteString (ByteString)

import DNIWE.Punt.Interface.Online

gameServer :: ByteString
gameServer = "punter.inf.ed.ac.uk"

main :: IO ()
main = do
  [portString] <- getArgs
  let port = read portString
  runJSONClient gameServer port (return ())
