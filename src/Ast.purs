module Ast
  ( CanonExpression(..)
  , Expression(..)
  , canonicalize
  , prettyPrint
  ) where

import Prelude

import Data.Foldable as Foldable
import Data.List (List(..), (:))
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Tokenize (Identifier)

data Expression
  = Variable Identifier
  | Abstraction Identifier Expression
  | Application Expression Expression

data CanonExpression
  = CVariable Int
  | CAbstraction CanonExpression
  | CApplication CanonExpression CanonExpression

derive instance Eq Expression
derive instance Eq CanonExpression

instance Show Expression where
  show = case _ of
    Variable identifier -> "(Variable " <> show identifier <> ")"
    Abstraction parameter body -> "(Abstraction " <> show parameter <> " " <> show body <> ")"
    Application operator operand -> "(Application " <> show operator <> " " <> show operand <> ")"

instance Show CanonExpression where
  show = case _ of
    CVariable identifier -> "(Variable " <> show identifier <> ")"
    CAbstraction body -> "(Abstraction " <> show body <> ")"
    CApplication operator operand -> "(Application " <> show operator <> " " <> show operand <> ")"

prettyPrint :: Expression -> String
prettyPrint = case _ of
  Variable identifier -> show identifier
  Abstraction parameter body -> "\\" <> show parameter <> "." <> show body
  Application operator operand -> "(" <> show operator <> " " <> show operand <> ")"

canonicalize :: Expression -> Maybe CanonExpression
canonicalize e =
  let
    canonicalizeHelper :: Expression -> List (Tuple Identifier Int) -> Int -> Maybe CanonExpression
    canonicalizeHelper expr bindings depth = case expr of
      Variable id -> CVariable <$> Foldable.lookup id bindings
      Abstraction id body -> CAbstraction <$> canonicalizeHelper body ((id /\ depth) : bindings) (depth + 1)
      Application operator operand ->
        CApplication <$> canonicalizeHelper operator bindings depth <*> canonicalizeHelper operand bindings depth
  in
    canonicalizeHelper e Nil 0