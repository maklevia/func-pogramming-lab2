{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (IOException, SomeException, catch, evaluate, try)
import Control.Monad (forM_, replicateM, when)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (getNumCapabilities)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, hSetEncoding, stderr, stdout, utf8)
import System.Mem (performGC)
import Text.Printf (printf)

import CLI
import SLAE.Gauss (residualNorm, solveSequential)
import SLAE.Generator (GeneratedSystem (..), generateSystem)
import SLAE.Parallel (solveParallel)
import SLAE.Parser (parseSLAE)
import SLAE.Types

data Problem = Problem
  { problemSLAE :: SLAE
  , problemExpected :: Maybe [Double]
  , problemDescription :: String
  }

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  args <- getArgs
  exitCode <- case parseCommand args of
    Left message -> do
      hPutStrLn stderr ("Помилка: " ++ message)
      hPutStrLn stderr ""
      hPutStrLn stderr usageText
      pure (ExitFailure 1)
    Right ShowHelp -> do
      putStrLn usageText
      pure ExitSuccess
    Right (Run options) -> catchTopLevel (run options)
  exitWith exitCode
  where
    catchTopLevel :: IO ExitCode -> IO ExitCode
    catchTopLevel action = action `catch` handler

    handler :: SomeException -> IO ExitCode
    handler exception = do
      hPutStrLn stderr ("Непередбачена помилка: " ++ show exception)
      pure (ExitFailure 1)

run :: Options -> IO ExitCode
run options = do
  loaded <- loadProblem options
  case loaded of
    Left message -> do
      hPutStrLn stderr ("Помилка: " ++ message)
      pure (ExitFailure 1)
    Right problem ->
      case optionMode options of
        BenchmarkMode -> runBenchmark options problem
        ParallelMode -> runSingle options problem
        SequentialMode -> runSingle options problem

loadProblem :: Options -> IO (Either String Problem)
loadProblem options =
  case optionGenerate options of
    Just size ->
      let generated = generateSystem size (optionSeed options)
      in pure
           ( Right
               Problem
                 { problemSLAE = generatedSLAE generated
                 , problemExpected = Just (generatedExpected generated)
                 , problemDescription =
                     "згенерована система: n="
                       ++ show size
                       ++ ", seed="
                       ++ show (optionSeed options)
                 }
           )
    Nothing -> do
      inputResult <- readInput (optionFile options)
      pure $ do
        input <- inputResult
        slae <- mapLeft renderInputError (parseSLAE input)
        pure
          Problem
            { problemSLAE = slae
            , problemExpected = Nothing
            , problemDescription = case optionFile options of
                Just path -> "файл " ++ path
                Nothing -> "stdin"
            }

readInput :: Maybe FilePath -> IO (Either String String)
readInput Nothing = Right <$> getContents
readInput (Just path) = do
  result <- (try $ do
    content <- readFile path
    _ <- evaluate (length content)
    pure content) :: IO (Either IOException String)
  pure $ case result of
    Left exception ->
      Left ("не вдалося прочитати файл «" ++ path ++ "»: " ++ show exception)
    Right content -> Right content

runSingle :: Options -> Problem -> IO ExitCode
runSingle options problem = do
  capabilities <- getNumCapabilities
  outcome <-
    try (timedSolve options (problemSLAE problem))
      :: IO (Either SomeException (Either SolveError [Double], Double))
  case outcome of
    Left exception -> do
      hPutStrLn stderr ("Помилка виконання: " ++ show exception)
      pure (ExitFailure 1)
    Right (result, seconds) ->
      case result of
        Left err -> printSolveFailure err
        Right solution -> do
          printSolution (problemSLAE problem) solution
          let residual = residualNorm (problemSLAE problem) solution
          printf "Нев'язка max|Ax-b| = %.3e\n" residual
          printf "Час розв'язку: %.3f мс\n" (seconds * 1000.0)
          case optionMode options of
            ParallelMode -> do
              printf "Доступно capabilities: %d\n" capabilities
              when (capabilities <= 1) $
                hPutStrLn
                  stderr
                  "Увага: доступний лише один capability; запустіть із +RTS -N, щоб увімкнути паралелізм."
            _ -> pure ()
          case problemExpected problem of
            Just expected ->
              printf
                "Макс. відхилення від відомого розв'язку: %.3e\n"
                (maxAbsDiff solution expected)
            Nothing -> pure ()
          pure ExitSuccess

runBenchmark :: Options -> Problem -> IO ExitCode
runBenchmark options problem = do
  capabilities <- getNumCapabilities
  let eps = optionEps options
      slae = problemSLAE problem
      repeats = optionRepeat options

  _ <- timeAction (pure (solveSequential eps slae))
  _ <- timeAction (solveParallel eps slae)

  sequentialRuns <- replicateM repeats (timeAction (pure (solveSequential eps slae)))
  parallelRuns <- replicateM repeats (timeAction (solveParallel eps slae))

  case (sequentialRuns, parallelRuns) of
    ((sequentialResult, _) : _, (parallelResult, _) : _) ->
      case (sequentialResult, parallelResult) of
        (Left err, _) -> printSolveFailure err
        (_, Left err) -> printSolveFailure err
        (Right sequentialSolution, Right parallelSolution) -> do
          let bestSequential = minimum (map snd sequentialRuns)
              bestParallel = minimum (map snd parallelRuns)
              speedup =
                if slaeSize slae >= 100
                    && bestSequential >= 1.0e-4
                    && bestParallel > 0
                  then Just (bestSequential / bestParallel)
                  else Nothing
              difference = maxAbsDiff parallelSolution sequentialSolution
              tolerance = 1e-9 * max 1.0 (maximum (map abs parallelSolution))
              residual = residualNorm slae parallelSolution

          printf "Система: %s\n" (problemDescription problem)
          printf "Розмір: n=%d\n" (slaeSize slae)
          printf "Доступно capabilities: %d\n" capabilities
          printf "Повторів: %d (береться найкращий час)\n\n" repeats
          printf "%-14s %14s %12s\n" ("Режим" :: String) ("Час, мс" :: String) ("Прискорення" :: String)
          printf
            "%-14s %14.3f %11.2fx\n"
            ("Послідовний" :: String)
            (bestSequential * 1000.0)
            (1.0 :: Double)
          case speedup of
            Just value ->
              printf
                "%-14s %14.3f %11.2fx\n"
                ("Паралельний" :: String)
                (bestParallel * 1000.0)
                (value :: Double)
            Nothing ->
              printf
                "%-14s %14.3f %11s\n"
                ("Паралельний" :: String)
                (bestParallel * 1000.0)
                ("n/a" :: String)
          printf "\nНев'язка max|Ax-b| = %.3e\n" residual
          printf "Макс. |x_par - x_seq| = %.3e\n" difference
          case problemExpected problem of
            Just expected ->
              printf
                "Макс. |x_par - x_known| = %.3e\n"
                (maxAbsDiff parallelSolution expected)
            Nothing -> pure ()

          case speedup of
            Just value
              | value < 1.0 ->
                  hPutStrLn
                    stderr
                    "Примітка: на малій системі накладні витрати можуть перевищувати виграш."
            Nothing ->
              hPutStrLn
                stderr
                "Примітка: система занадто мала для вимірювання прискорення."
            _ -> pure ()

          if difference > tolerance
            then do
              hPutStrLn stderr "ПОМИЛКА: паралельний і послідовний розв'язки розійшлися."
              pure (ExitFailure 2)
            else pure ExitSuccess
    _ -> do
      hPutStrLn stderr "Помилка: не вдалося виконати benchmark."
      pure (ExitFailure 1)

timedSolve :: Options -> SLAE -> IO (Either SolveError [Double], Double)
timedSolve options slae =
  timeAction $ case optionMode options of
    SequentialMode -> pure (solveSequential (optionEps options) slae)
    ParallelMode -> solveParallel (optionEps options) slae
    BenchmarkMode ->
      pure (Left (ShapeError "режим benchmark не використовує timedSolve"))

timeAction
  :: IO (Either SolveError [Double])
  -> IO (Either SolveError [Double], Double)
timeAction action = do
  performGC
  start <- getMonotonicTimeNSec
  result <- action
  forceSolveResult result
  end <- getMonotonicTimeNSec
  pure (result, nanosecondsToSeconds (end - start))

forceSolveResult :: Either SolveError [Double] -> IO ()
forceSolveResult result =
  case result of
    Left err -> evaluate (length (renderSolveError err)) >> pure ()
    Right solution -> evaluate (force solution) >> pure ()

printSolution :: SLAE -> [Double] -> IO ()
printSolution slae solution = do
  printf "Розв'язок (n=%d):\n" (slaeSize slae)
  forM_ (zip [1 :: Int ..] solution) $ \(index, value) ->
    printf "  x%d = %.12g\n" index value

printSolveFailure :: SolveError -> IO ExitCode
printSolveFailure err = do
  hPutStrLn stderr ("Помилка розв'язання: " ++ renderSolveError err)
  pure (ExitFailure 2)

nanosecondsToSeconds :: Word64 -> Double
nanosecondsToSeconds nanoseconds = fromIntegral nanoseconds / 1.0e9

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft function (Left value) = Left (function value)
mapLeft _ (Right value) = Right value
