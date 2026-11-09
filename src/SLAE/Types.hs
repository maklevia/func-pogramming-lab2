module SLAE.Types
  ( Row
  , Matrix
  , SLAE (..)
  , mkSLAE
  , InputError (..)
  , SolveError (..)
  , renderInputError
  , renderSolveError
  , isFiniteDouble
  , maxAbsDiff
  ) where

import Data.Array.Unboxed (UArray, listArray)

-- | One row of the augmented matrix: @n@ coefficients followed by the right-hand side.
--
-- Rows are unboxed arrays, which keeps the solver compact and avoids building
-- millions of list cons cells during elimination.
type Row = UArray Int Double

-- | Augmented matrix stored row by row.
type Matrix = [Row]

-- | A square system represented by an augmented matrix of size @n x (n + 1)@.
data SLAE = SLAE
  { slaeSize :: Int
  , slaeMatrix :: Matrix
  }
  deriving (Eq, Show)

-- | Build a system from ordinary lists. Every row is expected to contain
-- @n + 1@ values; the parser validates that before calling this function.
mkSLAE :: Int -> [[Double]] -> SLAE
mkSLAE n rows = SLAE n (map (listArray (0, n)) rows)

-- | Errors produced while reading a system from text.
data InputError
  = EmptyInput
  | InvalidSize Int String
  | TooManyUnknowns Int
  | WrongValueCount
      { inputSize :: Int
      , expectedValues :: Int
      , actualValues :: Int
      }
  | InvalidNumber Int String
  | NonFiniteNumber Int String
  deriving (Eq, Show)

-- | Errors produced while solving a system.
data SolveError
  = SingularPivot
      { pivotStep :: Int
      , pivotValue :: Double
      }
  | ShapeError String
  | NonFiniteResult String
  deriving (Eq, Show)

renderInputError :: InputError -> String
renderInputError err = case err of
  EmptyInput ->
    "вхідні дані порожні; очікується розмір системи n та n*(n+1) чисел"
  InvalidSize lineNo token ->
    "рядок " ++ show lineNo ++ ": розмір системи має бути додатним цілим числом, отримано «" ++ token ++ "»"
  TooManyUnknowns n ->
    "розмір системи n=" ++ show n ++ " завеликий; підтримується не більше 10000"
  WrongValueCount n expected actual ->
    "для n=" ++ show n ++ " очікується " ++ show expected
      ++ " чисел, отримано " ++ show actual
  InvalidNumber lineNo token ->
    "рядок " ++ show lineNo ++ ": не вдалося розпізнати число «" ++ token ++ "»"
  NonFiniteNumber lineNo token ->
    "рядок " ++ show lineNo ++ ": число «" ++ token ++ "» не є скінченним (NaN або Infinity)"

renderSolveError :: SolveError -> String
renderSolveError err = case err of
  SingularPivot step value ->
    "матриця вироджена або система не має єдиного розв'язку: "
      ++ "на кроці " ++ show (step + 1) ++ " провідний елемент дорівнює "
      ++ show value ++ " (за модулем менше eps)"
  ShapeError message ->
    "некоректна структура матриці: " ++ message
  NonFiniteResult message ->
    "обчислення дали NaN або нескінченність: " ++ message

isFiniteDouble :: Double -> Bool
isFiniteDouble value = not (isNaN value || isInfinite value)

-- | Maximum absolute difference between two vectors.
-- Returns infinity when the vectors have different lengths.
maxAbsDiff :: [Double] -> [Double] -> Double
maxAbsDiff xs ys
  | length xs /= length ys = 1 / 0
  | otherwise = maximum (0 : zipWith (\x y -> abs (x - y)) xs ys)
