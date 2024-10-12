module Parse where

import Prelude

import Ast (Expression(..))
import Control.Alt ((<|>))
import Control.Lazy as Control
import Control.Lazy as Control.Lazy
import Data.Array as Array
import Data.List (List)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe as Maybe
import Data.String.CodeUnits as String
import Parsing (Parser)
import Parsing (fail, liftMaybe) as Parsing
import Parsing.Combinators (between, chainl, chainl1, choice, many1, optionMaybe) as Parsing
import Parsing.String (char, satisfy) as Parsing
import Parsing.String.Basic (skipSpaces, space) as Parsing
import Parsing.Token (eof, match, token, when) as Parsing
import Tokenize (Identifier, Token, TokenElement(..), identifier, isName)

type Tokens = List Token

parser :: Parser Tokens Expression
parser = do
  expr <- expression
  Parsing.eof
  pure expr

expression :: Parser Tokens Expression
expression = Parsing.choice
  [ variable
  , Control.Lazy.defer \_ -> abstraction
  , Control.Lazy.defer \_ -> (applicationWithParens <|> applicationWithoutParens)
  ]

applicationWithoutParens :: Parser Tokens Expression
applicationWithoutParens = do
  e1 <- Control.Lazy.defer \_ -> expression
  e2 <- Control.Lazy.defer \_ -> expression
  pure $ Application e1 e2

applicationWithParens :: Parser Tokens Expression
applicationWithParens = Parsing.between (match LeftParen) (match RightParen) (Control.Lazy.defer \_ -> applicationWithoutParens)

abstraction :: Parser Tokens Expression
abstraction = do
  _ <- match Lambda
  signature :: (Expression -> Expression) <-
    Parsing.chainl1 (Abstraction <$> name) (pure compose)
  _ <- match Dot
  body <- expression
  pure $ signature body

variable :: Parser Tokens Expression
variable = Variable <$> name

name :: Parser Tokens Identifier
name = do
  { element } <- Parsing.when _.pos isName
  case element of
    Name ident -> pure ident
    _ -> Parsing.fail $ "Expected a Name, found " <> show element

match :: TokenElement -> Parser Tokens Unit
match x = do
  void $ Parsing.when _.pos (_.element >>> eq x)