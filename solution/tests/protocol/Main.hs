{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Data.Text as T

import Test.Hspec (Expectation, Spec, hspec, describe, it, shouldSatisfy, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, Arbitrary(..), sized, resize, oneof, choose, suchThat, frequency, arbitrarySizedNatural, listOf, listOf1, elements)



main :: IO ()
main = hspec spec

spec :: Spec
spec = return ()

  -- do
  -- describe "pgTag" $ do
  --   it $
  --   prop $
