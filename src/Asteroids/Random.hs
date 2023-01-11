{-# LANGUAGE RecordWildCards #-}

module Asteroids.Random where

import Data.Bifunctor
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.Random.Mersenne.Pure64

data PseudoRandom a
  = PseudoRandom
  { pseudoRandomValues :: Vector a
  , pseudoRandomIndex  :: Int
  }
  deriving (Eq, Show)

emptyPseudoRandom :: PseudoRandom a
emptyPseudoRandom = PseudoRandom V.empty 0

generatePseudoFloats :: Int -> IO (PseudoRandom Float)
generatePseudoFloats num = do
  gen <- newPureMT
  let fs = go num [] gen
      vs = V.fromList fs
  pure $ PseudoRandom vs 0
  where
    go 0 acc _ = acc
    go n acc g =
      let (d, nextGen) = randomDouble g
      in go (n - 1) (realToFrac d : acc) nextGen

getPseudoValue :: PseudoRandom a -> (Maybe a, PseudoRandom a)
getPseudoValue PseudoRandom {..} =
  let v = pseudoRandomValues V.!? pseudoRandomIndex
  in (v, PseudoRandom pseudoRandomValues ((pseudoRandomIndex + 1) `mod` V.length pseudoRandomValues))

getPseudoValues :: Show a => Int -> PseudoRandom a -> ([a], PseudoRandom a)
getPseudoValues count pseudo = first catMaybes $ go [] pseudo count
  where
    go :: Show a => [Maybe a] -> PseudoRandom a -> Int -> ([Maybe a], PseudoRandom a)
    go acc p n
      | n < 0 = ([], p)
      | n > 0 = let (v, p') = getPseudoValue p
                in go (acc ++ [v]) p' (n - 1)
      | otherwise = (acc, p)
