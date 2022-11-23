import Data.Proxy
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Classes

import Asteroids.Game

main :: IO ()
main = hspec $ do
  describe "KeyState" $ do
    it "is a Semigroup" $ do
      lawsCheck $ semigroupLaws (Proxy :: Proxy KeyState)

instance Arbitrary KeyState where
  arbitrary = oneof [pure KeyDown, pure KeyRelease]
