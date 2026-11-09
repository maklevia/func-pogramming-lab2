module SLAE.Generator
  ( GeneratedSystem (..)
  , generateSystem
  , maxGeneratedSize
  ) where

import Data.Bits ((.&.))
import Data.List (mapAccumL)
import Data.Word (Word64)

import SLAE.Types

-- | A generated system together with the solution used to build its
-- right-hand side.
data GeneratedSystem = GeneratedSystem
  { generatedSLAE :: SLAE
  , generatedExpected :: [Double]
  }
  deriving (Eq, Show)

-- | The generator builds an @n x n@ matrix, so keep it within a size that is
-- reasonable for a lab machine.
maxGeneratedSize :: Int
maxGeneratedSize = 2000

-- | Build a strictly diagonally dominant system with a known solution.
--
-- The off-diagonal coefficients are pseudo-random values in @[-1, 1]@. The
-- diagonal is larger than the sum of absolute off-diagonal values, which
-- guarantees that the matrix is nonsingular.
generateSystem :: Int -> Word64 -> GeneratedSystem
generateSystem n seed
  | n <= 0 = GeneratedSystem (mkSLAE 0 []) []
  | otherwise =
      let expected = [1.0 + fromIntegral i / fromIntegral n | i <- [0 .. n - 1]]
          (_, rows) = mapAccumL (generateRow n expected) seed [0 .. n - 1]
      in GeneratedSystem (mkSLAE n rows) expected

generateRow :: Int -> [Double] -> Word64 -> Int -> (Word64, [Double])
generateRow n expected state index =
  let (offDiagonal, state') = generateValues (n - 1) state
      diagonal = sum (map abs offDiagonal) + 1.0
      coefficients = insertAt index diagonal offDiagonal
      rhs = sum (zipWith (*) coefficients expected)
  in (state', coefficients ++ [rhs])

generateValues :: Int -> Word64 -> ([Double], Word64)
generateValues count initial = go count initial []
  where
    go 0 state values = (reverse values, state)
    go remaining state values =
      let state' = nextState state
          value = randomDouble state'
      in go (remaining - 1) state' (value : values)

nextState :: Word64 -> Word64
nextState state = (1103515245 * state + 12345) .&. 0x7fffffff

randomDouble :: Word64 -> Double
randomDouble state =
  let unit = fromIntegral (state .&. 0x7fffffff) / fromIntegral (0x7fffffff :: Word64)
  in unit * 2.0 - 1.0

insertAt :: Int -> a -> [a] -> [a]
insertAt index value xs =
  let (before, after) = splitAt index xs
  in before ++ value : after
