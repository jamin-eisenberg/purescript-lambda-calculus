module Ast
  ( Expression(..)
  , Identifier
  , isLegalIdentifierChar
  , mkIdentifier
  , prettyPrint
  ) where

import Prelude

import Data.Array as Array
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits as String.CodePoint

newtype Identifier = Identifier String

derive newtype instance Show Identifier
derive newtype instance Eq Identifier

mkIdentifier :: String -> Maybe Identifier
mkIdentifier s = if not String.null s && allCharsLegal then Just $ Identifier s else Nothing
  where
  allCharsLegal = Array.all isLegalIdentifierChar $ String.CodePoint.toCharArray s

isLegalIdentifierChar :: Char -> Boolean
isLegalIdentifierChar c = not $ Array.elem c [ '\\', '\r', '\n', '\t', ' ', '(', ')', '.' ]

data Expression
  = Variable Identifier
  | Abstraction Identifier Expression
  | Application Expression Expression

derive instance Eq Expression

instance Show Expression where
  show = case _ of
    Variable identifier -> "(Variable " <> show identifier <> ")"
    Abstraction parameter body -> "(Abstraction " <> show parameter <> " " <> show body <> ")"
    Application operator operand -> "(Application " <> show operator <> " " <> show operand <> ")"

prettyPrint :: Expression -> String
prettyPrint = case _ of
  Variable identifier -> show identifier
  Abstraction parameter body -> "\\" <> show parameter <> "." <> show body
  Application operator operand -> "(" <> show operator <> " " <> show operand <> ")"

