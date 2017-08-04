{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.ByteString.Lazy (ByteString)
import Data.Char (chr)
import Data.Text (Text)
import Data.Proxy (Proxy(..))
import Data.Maybe (isJust)
import Data.Aeson (FromJSON, Value, encode, decode, object, (.=))
import qualified Data.Text as T

import DNIWE.Punt.Interface.Protocol (HandshakeRequest)

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

newtype TestHandshakeRequest = TestHandshakeRequest Value deriving (Show)
instance Unwrap TestHandshakeRequest where unwrap (TestHandshakeRequest v) = v
instance Arbitrary TestHandshakeRequest where
  arbitrary = do
    name <- arbitraryString charASCII
    return $ TestHandshakeRequest (object [ "me" .= name ])


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

  describe "Setup" $ do
    prop "request parsing works" $ do
      pending

    prop "response parsing works" $ do
      pending

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

