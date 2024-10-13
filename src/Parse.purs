module Parse (parser) where

import Prelude

import Ast (Expression(..))
import Control.Lazy as Control.Lazy
import Data.List (List)
import Parsing (Parser)
import Parsing (fail) as Parsing
import Parsing.Combinators (between, chainl1, choice) as Parsing
import Parsing.Token (eof, when) as Parsing
import Tokenize (Identifier, Token, TokenElement(..), isName)

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
  , Control.Lazy.defer \_ -> applicationWithParens
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