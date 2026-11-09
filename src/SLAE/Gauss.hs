{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SLAE.Gauss
  ( solveSequential
  , residualNorm
  , validateSLAE
  ) where

import Control.Monad (forM, forM_)
import Control.Monad.ST (ST, runST)
import Data.Array.MArray (MArray, newArray, readArray, writeArray)
import Data.Array.ST (STUArray)
import Data.Array.Unboxed (bounds, elems, (!))

import SLAE.Types

-- | Solve a system sequentially with Gaussian elimination and partial pivoting.
--
-- The reference implementation also updates a mutable unboxed matrix in place.
-- This keeps the benchmark comparison fair: the parallel version differs only
-- in how the independent row updates are scheduled.
solveSequential :: Double -> SLAE -> Either SolveError [Double]
solveSequential eps slae = do
  validateSLAE slae
  runST (solveSequentialST eps slae)

solveSequentialST :: forall s. Double -> SLAE -> ST s (Either SolveError [Double])
solveSequentialST eps slae = do
  let n = slaeSize slae
  matrix <- toMutableST n (slaeMatrix slae)
  result <- eliminateSequentialM eps n matrix
  case result of
    Left err -> pure (Left err)
    Right () -> backSubstituteM eps n matrix

eliminateSequentialM
  :: MArray a Double m
  => Double
  -> Int
  -> a Int Double
  -> m (Either SolveError ())
eliminateSequentialM eps n matrix = go 0
  where
    go k
      | k >= n = pure (Right ())
      | otherwise = do
          pivot <- pickPivotM eps n k matrix
          case pivot of
            Left err -> pure (Left err)
            Right (pivotIndex, _) -> do
              swapRowsM n k k pivotIndex matrix
              forM_ [k + 1 .. n - 1] $ \rowIndex ->
                updateRowM n k rowIndex matrix
              go (k + 1)

validateSLAE :: SLAE -> Either SolveError ()
validateSLAE (SLAE n rows)
  | n <= 0 = Left (ShapeError "розмір системи має бути додатним")
  | length rows /= n =
      Left (ShapeError ("очікується " ++ show n ++ " рядків, отримано " ++ show (length rows)))
  | any (\row -> bounds row /= (0, n)) rows =
      Left (ShapeError ("кожен рядок має містити " ++ show (n + 1) ++ " чисел"))
  | any (not . all isFiniteDouble . elems) rows =
      Left (NonFiniteResult "матриця містить NaN або нескінченність")
  | otherwise = Right ()

-- | Copy the immutable rows into one flat mutable ST matrix of size
-- @n * (n + 1)@. Element @(i, j)@ lives at @i * (n + 1) + j@.
toMutableST :: Int -> Matrix -> ST s (STUArray s Int Double)
toMutableST n rows = do
  let stride = n + 1
  matrix <- newArray (0, n * stride - 1) 0.0
  forM_ (zip [0 ..] rows) $ \(rowIndex, row) ->
    forM_ [0 .. n] $ \column ->
      writeArray matrix (rowIndex * stride + column) (row ! column)
  pure matrix

-- | Find the row with the largest absolute value in column @k@.
pickPivotM
  :: MArray a Double m
  => Double
  -> Int
  -> Int
  -> a Int Double
  -> m (Either SolveError (Int, Double))
pickPivotM eps n k matrix = go k Nothing
  where
    stride = n + 1

    go i best
      | i >= n =
          case best of
            Nothing -> pure (Left (ShapeError "немає рядків для пошуку провідного елемента"))
            Just (index, value)
              | abs value < eps -> pure (Left (SingularPivot k value))
              | otherwise -> pure (Right (index, value))
      | otherwise = do
          value <- readArray matrix (i * stride + k)
          if not (isFiniteDouble value)
            then
              pure
                ( Left
                    ( NonFiniteResult
                        ("рядок " ++ show (i + 1) ++ ", стовпець " ++ show (k + 1))
                    )
                )
            else
              let best' = case best of
                    Nothing -> Just (i, value)
                    Just (_, current)
                      | abs value > abs current -> Just (i, value)
                      | otherwise -> best
              in go (i + 1) best'

-- | Swap rows @i@ and @j@ from column @start@ to the right-hand side.
swapRowsM
  :: MArray a Double m
  => Int
  -> Int
  -> Int
  -> Int
  -> a Int Double
  -> m ()
swapRowsM n start i j matrix
  | i == j = pure ()
  | otherwise = forM_ [start .. n] $ \column -> do
      let offsetI = i * stride + column
          offsetJ = j * stride + column
      valueI <- readArray matrix offsetI
      valueJ <- readArray matrix offsetJ
      writeArray matrix offsetI valueJ
      writeArray matrix offsetJ valueI
  where
    stride = n + 1

-- | Eliminate column @k@ in row @rowIndex@ using the pivot row.
updateRowM
  :: MArray a Double m
  => Int
  -> Int
  -> Int
  -> a Int Double
  -> m ()
updateRowM n k rowIndex matrix = do
  pivot <- readArray matrix (k * stride + k)
  rowPivot <- readArray matrix (rowIndex * stride + k)
  let factor = rowPivot / pivot
  forM_ [k + 1 .. n] $ \column -> do
    value <- readArray matrix (rowIndex * stride + column)
    pivotValue <- readArray matrix (k * stride + column)
    writeArray matrix (rowIndex * stride + column) (value - factor * pivotValue)
  writeArray matrix (rowIndex * stride + k) 0.0
  where
    stride = n + 1

-- | Back substitution for an upper-triangular augmented matrix.
backSubstituteM
  :: MArray a Double m
  => Double
  -> Int
  -> a Int Double
  -> m (Either SolveError [Double])
backSubstituteM eps n matrix = go (n - 1) []
  where
    stride = n + 1

    go i xs
      | i < 0 = pure (Right xs)
      | otherwise = do
          pivot <- readArray matrix (i * stride + i)
          rhs <- readArray matrix (i * stride + n)
          after <- forM [i + 1 .. n - 1] $ \column ->
            readArray matrix (i * stride + column)
          if abs pivot < eps
            then pure (Left (SingularPivot i pivot))
            else
              let subtotal = sum (zipWith (*) after xs)
                  value = (rhs - subtotal) / pivot
              in if not (isFiniteDouble value)
                   then pure (Left (NonFiniteResult ("x[" ++ show (i + 1) ++ "]")))
                   else go (i - 1) (value : xs)

-- | Maximum absolute residual @max_i |sum_j a_ij x_j - b_i|@.
residualNorm :: SLAE -> [Double] -> Double
residualNorm (SLAE n rows) solution =
  maximum (0 : map rowResidual rows)
  where
    rowResidual row =
      abs
        ( sum [row ! column * value | (column, value) <- zip [0 .. n - 1] solution]
            - row ! n
        )
