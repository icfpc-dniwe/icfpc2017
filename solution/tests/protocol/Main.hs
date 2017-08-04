{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Text (Text)
import qualified Data.Text as T

import DNIWE.Punt.Interface.Protocol (
    Direction(..)
  , Handshake(..))

import Test.Hspec (Expectation, Spec, hspec, describe, it, shouldSatisfy, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, Arbitrary(..), sized, resize, oneof, choose, suchThat, frequency, arbitrarySizedNatural, listOf, listOf1, elements)


-- inClass :: String -> Gen Char
-- inClass = oneof . inClass' where
--   inClass' (x:'-':y:xs) = (choose (x, y)):(inClass' xs)
--   inClass' (x:xs)       = (return x):(inClass' xs)
--   inClass' []           = []
--
-- charASCII :: Gen Char
-- charASCII = inClass [chr 32, '-', chr 127]
--
--
-- arbitraryString :: Gen Char -> Gen String
-- arbitraryString c = sized $ \n -> sequence . replicate n $ c
--
--
--
-- instance Arbitrary Direction where
--   arbitrary = elements [FromServer, ToServer]
--
-- instance Arbitrary Handshake where
--   arbitrary = do
--     handshakeDirection <- arbitrary
--     handshakeName      <- concatMap (\c -> if c `elem` "\"\\" then ['\\', c] else [c]) <$> (arbitraryString charASCII)
--     return Handshake {..}



main :: IO ()
main = hspec spec

spec :: Spec
spec = return ()
