module SLAE.Parser
  ( parseSLAE
  , maxSupportedSize
  ) where

import Text.Read (readMaybe)

import SLAE.Types

-- | Upper bound that keeps the parser and solver within reasonable memory.
maxSupportedSize :: Int
maxSupportedSize = 10000

-- | Parse an augmented matrix from text.
--
-- Format:
--
-- > n
-- > a11 a12 ... a1n b1
-- > ...
-- > an1 an2 ... ann bn
--
-- Whitespace is free-form, blank lines are ignored, and everything after
-- a @#@ character on a line is treated as a comment.
parseSLAE :: String -> Either InputError SLAE
parseSLAE rawInput =
  case tokenize (stripBom rawInput) of
    [] -> Left EmptyInput
    (lineNo, sizeToken) : valueTokens ->
      case readMaybe sizeToken :: Maybe Int of
        Nothing -> Left (InvalidSize lineNo sizeToken)
        Just n
          | n <= 0 -> Left (InvalidSize lineNo sizeToken)
          | n > maxSupportedSize -> Left (TooManyUnknowns n)
          | otherwise ->
              let expected = n * (n + 1)
                  actual = length valueTokens
              in if actual /= expected
                   then Left (WrongValueCount n expected actual)
                   else do
                     values <- traverse parseValue valueTokens
                     pure (mkSLAE n (chunksOf (n + 1) values))
  where
    parseValue (lineNo, token) =
      case readMaybe token :: Maybe Double of
        Nothing -> Left (InvalidNumber lineNo token)
        Just value
          | isFiniteDouble value -> Right value
          | otherwise -> Left (NonFiniteNumber lineNo token)

stripBom :: String -> String
stripBom ('\65279' : rest) = rest
stripBom input = input

tokenize :: String -> [(Int, String)]
tokenize input =
  concatMap onLine (zip [1 ..] (lines input))
  where
    onLine (lineNo, rawLine) =
      [ (lineNo, token)
      | token <- words (takeWhile (/= '#') rawLine)
      ]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf size xs =
  let (chunk, rest) = splitAt size xs
  in chunk : chunksOf size rest
