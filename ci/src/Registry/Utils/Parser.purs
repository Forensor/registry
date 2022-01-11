module Registry.Utils.Parser where

import Registry.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.String.CodeUnits as CU
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser as SP
import Text.Parsing.StringParser.CodePoints as SPCP
import Text.Parsing.StringParser.Combinators as SPC

nullaryCtor :: forall a. String -> a -> Parser a
nullaryCtor str ctor = do
  _ <- SPCP.string str
  pure ctor

singleCtor :: forall a b. String -> (a -> b) -> Parser a -> Parser b
singleCtor str ctor parser = do
  _ <- SPCP.string str
  _ <- SPCP.whiteSpace
  contents <- parser
  pure $ ctor contents

parens :: forall a. Parser a -> Parser a
parens = SPC.between (SPCP.char '(' *> SPCP.whiteSpace) (SPCP.whiteSpace *> SPCP.char ')')

brackets :: forall a. Parser a -> Parser a
brackets = SPC.between (SPCP.char '[' *> SPCP.whiteSpace) (SPCP.whiteSpace *> SPCP.char ']')

array :: forall a. Parser a -> Parser (Array a)
array parser = do
  results <- brackets (SPC.sepBy parser (SPCP.char ',' *> SPCP.whiteSpace))
  pure $ Array.fromFoldable results

array1 :: forall a. Parser a -> Parser (NonEmptyArray a)
array1 parser = do
  results <- brackets (SPC.sepBy1 parser (SPCP.char ',' *> SPCP.whiteSpace))
  pure $ NEA.fromFoldable1 results

anyString :: Parser String
anyString = do
  captured <- SPC.many SPCP.anyChar
  pure $ CU.fromCharArray $ Array.fromFoldable captured

tryChoice :: forall a. Array (Parser a) -> Parser a
tryChoice = SPC.choice <<< map SP.try
