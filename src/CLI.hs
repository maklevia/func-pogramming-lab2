module CLI
  ( Command (..)
  , Mode (..)
  , Options (..)
  , defaultOptions
  , parseCommand
  , usageText
  ) where

import Data.List (stripPrefix)
import Data.Word (Word64)
import Text.Read (readMaybe)

import SLAE.Generator (maxGeneratedSize)
import SLAE.Types (isFiniteDouble)

data Mode
  = ParallelMode
  | SequentialMode
  | BenchmarkMode
  deriving (Eq, Show)

data Options = Options
  { optionMode :: Mode
  , optionEps :: Double
  , optionRepeat :: Int
  , optionGenerate :: Maybe Int
  , optionSeed :: Word64
  , optionFile :: Maybe FilePath
  }
  deriving (Eq, Show)

data Command
  = ShowHelp
  | Run Options
  deriving (Eq, Show)

defaultOptions :: Options
defaultOptions =
  Options
    { optionMode = ParallelMode
    , optionEps = 1e-12
    , optionRepeat = 3
    , optionGenerate = Nothing
    , optionSeed = 42
    , optionFile = Nothing
    }

data ParserState = ParserState
  { parserOptions :: Options
  , parserModes :: [Mode]
  }

parseCommand :: [String] -> Either String Command
parseCommand args
  | any isHelp args = Right ShowHelp
  | otherwise = Run <$> (finish =<< go initialState args)
  where
    initialState = ParserState defaultOptions []
    isHelp arg = arg == "--help" || arg == "-h"

    finish state =
      let options = parserOptions state
          modes = parserModes state
          selectedMode = case modes of
            [] -> Right ParallelMode
            [mode] -> Right mode
            _ | SequentialMode `elem` modes && BenchmarkMode `elem` modes ->
                  Left "не можна поєднувати --seq і --bench"
              | otherwise -> Right (head modes)
      in do
        mode <- selectedMode
        case (optionGenerate options, optionFile options) of
          (Just _, Just _) ->
            Left "--gen не можна поєднувати з файлом або stdin; згенеруйте систему без вхідного файлу"
          _ -> Right options {optionMode = mode}

go :: ParserState -> [String] -> Either String ParserState
go state [] = Right state
go state (arg : rest)
  | arg == "--seq" = go state {parserModes = SequentialMode : parserModes state} rest
  | arg == "--bench" = go state {parserModes = BenchmarkMode : parserModes state} rest
  | arg == "--eps" = do
      (value, rest') <- needValue "--eps" rest
      eps <- parseEps value
      go state {parserOptions = (parserOptions state) {optionEps = eps}} rest'
  | Just value <- stripPrefix "--eps=" arg = do
      eps <- parseEps value
      go state {parserOptions = (parserOptions state) {optionEps = eps}} rest
  | arg == "--repeat" = do
      (value, rest') <- needValue "--repeat" rest
      repeatCount <- parseRepeat value
      go state {parserOptions = (parserOptions state) {optionRepeat = repeatCount}} rest'
  | Just value <- stripPrefix "--repeat=" arg = do
      repeatCount <- parseRepeat value
      go state {parserOptions = (parserOptions state) {optionRepeat = repeatCount}} rest
  | arg == "--gen" = do
      (value, rest') <- needValue "--gen" rest
      size <- parseGen value
      go state {parserOptions = (parserOptions state) {optionGenerate = Just size}} rest'
  | Just value <- stripPrefix "--gen=" arg = do
      size <- parseGen value
      go state {parserOptions = (parserOptions state) {optionGenerate = Just size}} rest
  | arg == "--seed" = do
      (value, rest') <- needValue "--seed" rest
      seed <- parseSeed value
      go state {parserOptions = (parserOptions state) {optionSeed = seed}} rest'
  | Just value <- stripPrefix "--seed=" arg = do
      seed <- parseSeed value
      go state {parserOptions = (parserOptions state) {optionSeed = seed}} rest
  | arg == "--" = addFiles state rest
  | arg == "-" = go state rest
  | take 2 arg == "--" = Left ("невідома опція «" ++ arg ++ "»")
  | otherwise = do
      state' <- addFile state arg
      go state' rest

addFiles :: ParserState -> [String] -> Either String ParserState
addFiles state [] = Right state
addFiles state (path : rest) = do
  state' <- addFile state path
  addFiles state' rest

addFile :: ParserState -> FilePath -> Either String ParserState
addFile state path =
  case optionFile (parserOptions state) of
    Just _ -> Left "можна вказати лише один вхідний файл"
    Nothing ->
      Right state {parserOptions = (parserOptions state) {optionFile = Just path}}

needValue :: String -> [String] -> Either String (String, [String])
needValue name [] = Left ("опція " ++ name ++ " потребує значення")
needValue _ (value : rest) = Right (value, rest)

parseEps :: String -> Either String Double
parseEps token =
  case readMaybe token :: Maybe Double of
    Just value
      | isFiniteDouble value && value > 0 -> Right value
    _ -> Left ("--eps має бути додатним скінченним числом, отримано «" ++ token ++ "»")

parseRepeat :: String -> Either String Int
parseRepeat token =
  case readMaybe token :: Maybe Int of
    Just value
      | value >= 1 && value <= 1000 -> Right value
    _ -> Left ("--repeat має бути цілим числом від 1 до 1000, отримано «" ++ token ++ "»")

parseGen :: String -> Either String Int
parseGen token =
  case readMaybe token :: Maybe Int of
    Just value
      | value >= 1 && value <= maxGeneratedSize -> Right value
    _ ->
      Left
        ( "--gen має бути цілим числом від 1 до "
            ++ show maxGeneratedSize
            ++ ", отримано «"
            ++ token
            ++ "»"
        )

parseSeed :: String -> Either String Word64
parseSeed token =
  case readMaybe token :: Maybe Word64 of
    Just value -> Right value
    Nothing -> Left ("--seed має бути невід'ємним цілим числом, отримано «" ++ token ++ "»")

usageText :: String
usageText =
  unlines
    [ "Використання: parallel-gauss-solver [ОПЦІЇ] [ФАЙЛ]"
    , ""
    , "Розв'язує систему лінійних рівнянь A x = b методом Гаусса"
    , "з частковим вибором головного елемента."
    , ""
    , "Режими:"
    , "  (типово)        паралельний розв'язок"
    , "  --seq           послідовний розв'язок"
    , "  --bench         порівняти послідовний і паралельний режими"
    , ""
    , "Опції:"
    , "  --eps EPS       поріг виродженості, типово 1e-12"
    , "  --repeat N      кількість повторів для --bench, типово 3"
    , "  --gen N         згенерувати діагонально домінантну систему N x N"
    , "  --seed S        seed для --gen, типово 42"
    , "  --help, -h      показати цю довідку"
    , ""
    , "Вхідний формат (файл або stdin):"
    , "  n"
    , "  a11 a12 ... a1n b1"
    , "  ..."
    , "  an1 an2 ... ann bn"
    , ""
    , "Числа розділяються будь-якими пробільними символами;"
    , "порожні рядки ігноруються, текст після # є коментарем."
    , ""
    , "Приклади:"
    , "  parallel-gauss-solver examples/simple3.txt"
    , "  parallel-gauss-solver --seq examples/simple3.txt"
    , "  parallel-gauss-solver --bench --gen 600 --repeat 3 +RTS -N8"
    , "  cat examples/simple2.txt | parallel-gauss-solver"
    ]
