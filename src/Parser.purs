module Parser (parser) where

import Prelude

import Ast (Expression(..), Identifier, isLegalIdentifierChar, mkIdentifier)
import Control.Alt ((<|>))
import Control.Lazy as Control.Lazy
import Data.Array as Array
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe as Maybe
import Data.String.CodeUnits as String
import Parsing (Parser)
import Parsing (liftMaybe) as Parsing
import Parsing.Combinators (chainl1, choice, many1, optionMaybe) as Parsing
import Parsing.String (char, eof, satisfy) as Parsing
import Parsing.String.Basic (skipSpaces, space) as Parsing

parser :: Parser String Expression
parser = do
  Parsing.skipSpaces
  expr <- expression
  Parsing.skipSpaces
  Parsing.eof
  pure expr

expression :: Parser String Expression
expression = Parsing.choice
  [ variable
  , Control.Lazy.defer \_ -> abstraction
  , Control.Lazy.defer \_ -> application
  ]

application :: Parser String Expression
application = do
  firstParen <- Parsing.optionMaybe $ Parsing.char '('
  app <- Parsing.chainl1
    (variable <|> abstraction <|> application)
    ((Parsing.many1 $ Parsing.space) $> Application)
  when (Maybe.isJust firstParen)
    $ void
    $ Parsing.char ')'
  pure app

abstraction :: Parser String Expression
abstraction = do
  _ <- Parsing.char '\\'
  Parsing.skipSpaces
  signature :: (Expression -> Expression) <-
    Parsing.chainl1 (Abstraction <$> identifier) (Parsing.many1 Parsing.space $> compose)
  Parsing.skipSpaces
  _ <- Parsing.char '.'
  Parsing.skipSpaces
  body <- expression
  Parsing.skipSpaces
  pure $ signature body

variable :: Parser String Expression
variable = identifier <#> Variable

-- TODO λ as abstraction start and disallowed in ident
identifier :: Parser String Identifier
identifier = do
  identChars <- Parsing.many1 (Parsing.satisfy isLegalIdentifierChar)
  let
    ident = identChars
      # NonEmptyList.toList
      # Array.fromFoldable
      # String.fromCharArray
  ident
    # mkIdentifier
    # Parsing.liftMaybe (\_ -> "Illegal identifier: " <> ident)
