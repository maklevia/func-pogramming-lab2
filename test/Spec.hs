module Main (main) where

import Data.Array.Unboxed ((!))
import Data.Either (isLeft)
import System.Exit (exitFailure)

import CLI
import SLAE.Gauss (residualNorm, solveSequential)
import SLAE.Generator (GeneratedSystem (..), generateSystem)
import SLAE.Parallel (solveParallel)
import SLAE.Parser (parseSLAE)
import SLAE.Types

main :: IO ()
main = do
  check "parser accepts an augmented matrix" $
    parseSLAE "2\n1 1 3\n2 -1 0\n" == Right (mkSLAE 2 [[1, 1, 3], [2, -1, 0]])

  check "parser ignores comments and blank lines" $
    parseSLAE "# comment\n2\n1 1 3 # rhs\n\n2 -1 0\n"
      == Right (mkSLAE 2 [[1, 1, 3], [2, -1, 0]])

  check "parser rejects empty input" $
    parseSLAE "  \n# only a comment\n" == Left EmptyInput

  check "parser rejects non-positive size" $
    parseSLAE "0\n" == Left (InvalidSize 1 "0")

  check "parser rejects wrong value count" $
    parseSLAE "2\n1 2 3\n" == Left (WrongValueCount 2 6 3)

  check "parser rejects unknown token" $
    parseSLAE "1\nfoo 1\n" == Left (InvalidNumber 2 "foo")

  check "parser rejects non-finite values" $
    isLeft (parseSLAE "1\nNaN 1\n")

  check "solves a 2x2 system" $
    solveSequential 1e-12 (mkSLAE 2 [[1, 1, 3], [2, -1, 0]]) == Right [1.0, 2.0]

  check "solves a 3x3 system" $
    solutionApprox [2.0, 3.0, -1.0] $
      solveSequential 1e-12 (mkSLAE 3 [[2, 1, -1, 8], [-3, -1, 2, -11], [-2, 1, 2, -3]])

  check "solves a 1x1 system" $
    solveSequential 1e-12 (mkSLAE 1 [[2, 4]]) == Right [2.0]

  check "detects a singular matrix" $
    isLeft (solveSequential 1e-12 (mkSLAE 2 [[1, 1, 2], [2, 2, 4]]))

  check "residual is close to zero for a solved system" $
    let slae = mkSLAE 3 [[2, 1, -1, 8], [-3, -1, 2, -11], [-2, 1, 2, -3]]
    in case solveSequential 1e-12 slae of
         Right solution -> residualNorm slae solution < 1e-9
         Left _ -> False

  let generated = generateSystem 24 42
      generatedSLAEValue = generatedSLAE generated
      expected = generatedExpected generated

  check "generator is deterministic" $
    generateSystem 24 42 == generateSystem 24 42

  check "generated matrix is strictly diagonally dominant" $
    diagonallyDominant generatedSLAEValue

  check "generated system solves with a small error" $
    case solveSequential 1e-12 generatedSLAEValue of
      Right solution ->
        maxAbsDiff solution expected < 1e-8
          && residualNorm generatedSLAEValue solution < 1e-8
      Left _ -> False

  parallelResult <- solveParallel 1e-12 generatedSLAEValue
  check "parallel solver agrees with the sequential solver" $
    case (solveSequential 1e-12 generatedSLAEValue, parallelResult) of
      (Right sequentialSolution, Right parallelSolution) ->
        maxAbsDiff sequentialSolution parallelSolution < 1e-9
      _ -> False

  check "CLI defaults to parallel mode" $
    parseCommand [] == Right (Run defaultOptions)

  check "CLI parses --seq and a file" $
    parseCommand ["--seq", "examples/simple2.txt"]
      == Right
        ( Run
            defaultOptions
              { optionMode = SequentialMode
              , optionFile = Just "examples/simple2.txt"
              }
        )

  check "CLI parses --bench, --gen, --repeat and --seed" $
    parseCommand ["--bench", "--gen", "100", "--repeat", "2", "--seed", "7"]
      == Right
        ( Run
            defaultOptions
              { optionMode = BenchmarkMode
              , optionGenerate = Just 100
              , optionRepeat = 2
              , optionSeed = 7
              }
        )

  check "CLI supports --eps=value" $
    case parseCommand ["--eps=1e-9"] of
      Right (Run options) -> optionEps options == 1e-9
      _ -> False

  check "CLI rejects --seq together with --bench" $
    isLeft (parseCommand ["--seq", "--bench"])

  check "CLI rejects non-positive epsilon" $
    isLeft (parseCommand ["--eps", "0"])

  check "CLI rejects a non-positive generator size" $
    isLeft (parseCommand ["--gen", "0"])

  check "CLI rejects --gen together with a file" $
    isLeft (parseCommand ["--gen", "100", "examples/simple2.txt"])

  check "CLI rejects an unknown option" $
    isLeft (parseCommand ["--nope"])

  check "CLI rejects more than one file" $
    isLeft (parseCommand ["a.txt", "b.txt"])

  check "CLI shows help" $
    parseCommand ["--help"] == Right ShowHelp

  check "maxAbsDiff computes the maximum difference" $
    maxAbsDiff [1, 2, 3] [1, 4, 3] == 2

  putStrLn "Усі тести пройшли."

solutionApprox :: [Double] -> Either SolveError [Double] -> Bool
solutionApprox expected result =
  case result of
    Right actual ->
      length actual == length expected
        && and (zipWith (\expectedValue actualValue -> abs (expectedValue - actualValue) < 1e-9) expected actual)
    Left _ -> False

diagonallyDominant :: SLAE -> Bool
diagonallyDominant (SLAE n rows) =
  and
    [ abs (row ! index)
        > sum [abs (row ! position) | position <- [0 .. n - 1], position /= index]
    | (index, row) <- zip [0 ..] rows
    ]

check :: String -> Bool -> IO ()
check label ok =
  if ok
    then putStrLn ("PASS " ++ label)
    else do
      putStrLn ("FAIL " ++ label)
      exitFailure
