{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SLAE.Parallel
  ( solveParallel
  ) where

import Control.Concurrent
  ( Chan
  , MVar
  , forkFinally
  , newChan
  , newEmptyMVar
  , putMVar
  , readChan
  , takeMVar
  , writeChan
  )
import Control.Exception (SomeException, bracket, throwIO, try)
import Control.Monad (forM, forM_, replicateM, replicateM_)
import Data.Array.IO (IOUArray)
import Data.Array.MArray (newArray, readArray, writeArray)
import Data.Array.Unboxed ((!))
import GHC.Conc (getNumCapabilities)

import SLAE.Gauss (solveSequential, validateSLAE)
import SLAE.Types

-- | A persistent pool of worker threads. Creating a fresh pool of threads for
-- every elimination step would cost more than the row updates themselves, so
-- the pool is created once per solve and reused for all steps.
data WorkerPool = WorkerPool
  { poolQueue :: Chan Job
  , poolDone :: MVar ()
  , poolSize :: Int
  }

data Job
  = forall a. Job (IO a) (MVar (Either SomeException a))
  | Stop

-- | Solve a system with parallel forward elimination and sequential back
-- substitution.
--
-- The matrix is one flat mutable unboxed array. After the pivot row is chosen
-- and swapped, every row below the pivot is independent, so workers update
-- disjoint rows in place. The pool barrier between elimination steps makes
-- all writes visible before the next pivot search reads the matrix.
solveParallel :: Double -> SLAE -> IO (Either SolveError [Double])
solveParallel eps slae =
  case validateSLAE slae of
    Left err -> pure (Left err)
    Right () -> do
      workers <- getNumCapabilities
      if workers <= 1
        then pure (solveSequential eps slae)
        else
          withWorkerPool workers $ \pool -> do
            let n = slaeSize slae
            matrix <- toMutableIO n (slaeMatrix slae)
            result <- eliminateParallel pool eps n matrix
            case result of
              Left err -> pure (Left err)
              Right () -> backSubstituteIO eps n matrix

eliminateParallel
  :: WorkerPool
  -> Double
  -> Int
  -> IOUArray Int Double
  -> IO (Either SolveError ())
eliminateParallel pool eps n matrix = go 0
  where
    go k
      | k >= n = pure (Right ())
      | otherwise = do
          pivot <- pickPivotIO eps n k matrix
          case pivot of
            Left err -> pure (Left err)
            Right (pivotIndex, _) -> do
              swapRowsIO n k k pivotIndex matrix
              let rowIndices = [k + 1 .. n - 1]
                  cells = length rowIndices * (n - k + 1)
                  useParallel =
                    length rowIndices >= minParallelRows
                      && cells >= minParallelCells
              if useParallel
                then
                  parallelForMChunked
                    pool
                    minParallelRows
                    rowIndices
                    (\rowIndex -> updateRowIO n k rowIndex matrix)
                else forM_ rowIndices (\rowIndex -> updateRowIO n k rowIndex matrix)
              go (k + 1)

minParallelRows :: Int
minParallelRows = 32

minParallelCells :: Int
minParallelCells = 50000

-- | Run an action for every list element, distributing chunks over the pool.
-- The caller waits for every worker before returning, which is also the
-- barrier that makes the in-place writes visible to the next elimination step.
parallelForMChunked :: WorkerPool -> Int -> [a] -> (a -> IO ()) -> IO ()
parallelForMChunked pool minElements xs action
  | length xs < minElements = mapM_ action xs
  | otherwise = do
      let chunks = chunkInto (poolSize pool * 2) xs
      variables <- forM chunks $ \chunk -> do
        resultVariable <- newEmptyMVar
        writeChan (poolQueue pool) (Job (mapM_ action chunk) resultVariable)
        pure resultVariable
      outcomes <- mapM takeMVar variables
      case sequence outcomes of
        Left exception -> throwIO exception
        Right _ -> pure ()

chunkInto :: Int -> [a] -> [[a]]
chunkInto count xs
  | null xs = []
  | otherwise =
      let chunkSize = max 1 ((length xs + count - 1) `div` count)
      in chunksOf chunkSize xs

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf size xs =
  let (chunk, rest) = splitAt size xs
  in chunk : chunksOf size rest

withWorkerPool :: Int -> (WorkerPool -> IO a) -> IO a
withWorkerPool count action = bracket create shutdown action
  where
    create = do
      queue <- newChan
      done <- newEmptyMVar
      _ <- replicateM count $
        forkFinally (workerLoop queue) (\_ -> putMVar done ())
      pure (WorkerPool queue done count)

    shutdown pool = do
      replicateM_ (poolSize pool) (writeChan (poolQueue pool) Stop)
      replicateM_ (poolSize pool) (takeMVar (poolDone pool))

workerLoop :: Chan Job -> IO ()
workerLoop queue = go
  where
    go = do
      job <- readChan queue
      case job of
        Stop -> pure ()
        Job action resultVariable -> do
          outcome <- try action
          putMVar resultVariable outcome
          go

toMutableIO :: Int -> Matrix -> IO (IOUArray Int Double)
toMutableIO n rows = do
  let stride = n + 1
  matrix <- newArray (0, n * stride - 1) 0.0
  forM_ (zip [0 ..] rows) $ \(rowIndex, row) ->
    forM_ [0 .. n] $ \column ->
      writeArray matrix (rowIndex * stride + column) (row ! column)
  pure matrix

pickPivotIO
  :: Double
  -> Int
  -> Int
  -> IOUArray Int Double
  -> IO (Either SolveError (Int, Double))
pickPivotIO eps n k matrix = go k Nothing
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

swapRowsIO
  :: Int
  -> Int
  -> Int
  -> Int
  -> IOUArray Int Double
  -> IO ()
swapRowsIO n start i j matrix
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

updateRowIO
  :: Int
  -> Int
  -> Int
  -> IOUArray Int Double
  -> IO ()
updateRowIO n k rowIndex matrix = do
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
{-# INLINE updateRowIO #-}

backSubstituteIO
  :: Double
  -> Int
  -> IOUArray Int Double
  -> IO (Either SolveError [Double])
backSubstituteIO eps n matrix = go (n - 1) []
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
