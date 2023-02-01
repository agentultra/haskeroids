import Linear
import Data.Proxy
import qualified Data.Vector as V
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Classes

import Asteroids.Game

main :: IO ()
main = hspec $ do
  describe "KeyState" $
    it "is a Semigroup" $
      lawsCheck $ semigroupLaws (Proxy :: Proxy KeyState)

  -- describe "PseudoRandom" $ do
  --   describe "getPseudoValue" $ do
  --     it "should always return a value" $ do
  --       let p = PseudoRandom (V.fromList [1]) 0
  --           (r1, p') = getPseudoValue p
  --           (r2, p'') = getPseudoValue p'
  --           (r3, p''') = getPseudoValue p''
  --       [(r1, p'), (r2, p''), (r3, p''')] `shouldBe` [(Just 1, p), (Just 1, p), (Just 1, p)]

  --     it "the index should stay in range with a longer vector" $ do
  --       let p = PseudoRandom (V.fromList [1, 2]) 0
  --           (_, p') = getPseudoValue p
  --           (_, p'') = getPseudoValue p'
  --           (_, p''') = getPseudoValue p''
  --       pseudoRandomIndex p''' `shouldBe` 1

  --   describe "getPseudoValues" $ do
  --     it "should always return the requested number of elements" $ do
  --       let p = PseudoRandom (V.fromList [1, 2]) 0
  --       getPseudoValues 4 p `shouldBe` ([1, 2, 1, 2], p { pseudoRandomIndex = 0 })

  describe "isCollidingBox" $ do
    context "When the box is colliding on the left side of other" $ do
      it "should return True" $
        let box1 = CollisionBox (V2 1 0) (V2 3 3)
            box2 = CollisionBox (V2 2 0) (V2 3 3)
        in isCollidingBox box1 box2 `shouldBe` True
    context "When the box is colliding on the right side of other" $ do
      it "should return True" $
        let box1 = CollisionBox (V2 4 0) (V2 3 3)
            box2 = CollisionBox (V2 2 0) (V2 3 3)
        in isCollidingBox box1 box2 `shouldBe` True
    context "When the box is colliding on the top side of other" $ do
      it "should return True" $
        let box1 = CollisionBox (V2 0 1) (V2 3 3)
            box2 = CollisionBox (V2 0 2) (V2 3 3)
        in isCollidingBox box1 box2 `shouldBe` True
    context "When the box is colliding on the bottom side of other" $ do
      it "should return True" $
        let box1 = CollisionBox (V2 0 4) (V2 3 3)
            box2 = CollisionBox (V2 0 2) (V2 3 3)
        in isCollidingBox box1 box2 `shouldBe` True


instance Arbitrary KeyState where
  arbitrary = oneof [pure KeyDown, pure KeyRelease]
