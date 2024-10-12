module Ast
  ( Expression(..)
  , prettyPrint
  ) where

import Prelude

import Tokenize (Identifier)

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

