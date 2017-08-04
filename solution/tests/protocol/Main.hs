{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad (liftM2)
import Data.ByteString.Lazy (ByteString)
import Data.Char (chr)
import Data.Text (Text)
import Data.Proxy (Proxy(..))
import Data.Maybe (isJust)
import Data.Aeson (FromJSON, Value, encode, decode, object, (.=))
import qualified Data.Text as T

import DNIWE.Punt.Interface.Protocol (HandshakeRequest, HandshakeResponse, SetupRequest, SetupResponse)

import Test.Hspec (Expectation, Spec, hspec, describe, it, pending, shouldSatisfy, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, Arbitrary(..), sized, resize, oneof, choose, suchThat, frequency, arbitrarySizedNatural, listOf, listOf1, elements)


inClass :: String -> Gen Char
inClass = oneof . inClass' where
  inClass' (x:'-':y:xs) = (choose (x, y)):(inClass' xs)
  inClass' (x:xs)       = (return x):(inClass' xs)
  inClass' []           = []

charASCII :: Gen Char
charASCII = inClass [chr 32, '-', chr 127]

arbitraryString :: Gen Char -> Gen String
arbitraryString c = sized $ \n -> sequence . replicate n $ c


class (Show a) => Unwrap a where
  unwrap :: a -> Value

arbitraryPunterId :: Gen Int
arbitraryPunterId = arbitrarySizedNatural 

arbitrarySiteId :: Gen Int
arbitrarySiteId = arbitrarySizedNatural 
  

newtype TestHandshakeRequest = TestHandshakeRequest Value deriving (Show)
instance Unwrap TestHandshakeRequest where unwrap (TestHandshakeRequest v) = v
instance Arbitrary TestHandshakeRequest where
  arbitrary = do
    name <- arbitraryString charASCII
    return $ TestHandshakeRequest (object [ "me" .= name ])


newtype TestHandshakeResponse = TestHandshakeResponse Value deriving (Show)
instance Unwrap TestHandshakeResponse where unwrap (TestHandshakeResponse v) = v
instance Arbitrary TestHandshakeResponse where
  arbitrary = do
    name <- arbitraryString charASCII
    return $ TestHandshakeResponse (object [ "you" .= name ])


newtype TestSetupRequest = TestSetupRequest Value deriving (Show)
instance Unwrap TestSetupRequest where unwrap (TestSetupRequest v) = v
instance Arbitrary TestSetupRequest where
  arbitrary = do
    punter  <- arbitraryPunterId
    punters <- arbitraryPunterId

    sites  <- listOf1 arbitrarySiteId
    rivers <- listOf1 (liftM2 (,) (elements sites) (elements sites))
    mines  <- resize (length sites) $ listOf1 . elements $ sites

    name <- arbitraryString charASCII
    return $ TestSetupRequest (object [
        "punter" .= punter
      , "punters" .= punters
      , "map" .= object [
           "sites" .= map (\x -> object ["id" .= x]) sites
         , "rivers" .= map (\(a, b) -> object ["source" .= a, "target" .= b]) rivers
         , "mines" .= mines]])


newtype TestSetupResponse = TestSetupResponse Value deriving (Show)
instance Unwrap TestSetupResponse where unwrap (TestSetupResponse v) = v
instance Arbitrary TestSetupResponse where
  arbitrary = do
    punterId <- arbitraryPunterId
    return $ TestSetupResponse (object [ "ready" .= punterId ])

  
propParsing
  :: forall dataFrom dataTo. (Arbitrary dataFrom, Unwrap dataFrom, FromJSON dataTo, Show dataTo)
  => Proxy dataFrom
  -> Proxy dataTo
  -> Spec
propParsing _ _ = prop "parsing works" test where
  test :: dataFrom -> Expectation
  test = (`shouldSatisfy` isJust) . parse . encode . unwrap

  parse :: ByteString -> Maybe dataTo
  parse = decode


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "HandshakeRequest" $ do
    propParsing (Proxy :: Proxy TestHandshakeRequest) (Proxy :: Proxy HandshakeRequest)

  describe "HandshakeResponse" $ do
    propParsing (Proxy :: Proxy TestHandshakeResponse) (Proxy :: Proxy HandshakeResponse)

  describe "SetupRequest" $ do
    propParsing (Proxy :: Proxy TestSetupRequest) (Proxy :: Proxy SetupRequest)

  describe "SetupResponse" $ do
    propParsing (Proxy :: Proxy TestSetupResponse) (Proxy :: Proxy SetupResponse)

  
  describe "Gameplay" $ do
    prop "request parsing works" $ do
      pending

    prop "response parsing works" $ do
      pending

  describe "Scoring" $ do
    prop "request parsing works" $ do
      pending

    prop "response parsing works" $ do
      pending

