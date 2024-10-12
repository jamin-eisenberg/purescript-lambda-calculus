module Tokenize
  ( Identifier
  , Token
  , TokenElement(..)
  , identifier
  , isName
  , mkIdentifier
  , tokens
  ) where

import Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.String.CodeUnits as String.CodeUnits
import Parsing (Parser)
import Parsing (Position, liftMaybe, position) as Parsing
import Parsing.Combinators (choice, many, many1) as Parsing
import Parsing.String (satisfy, string) as Parsing
import Parsing.String.Basic (skipSpaces) as Parsing

newtype Identifier = Identifier String

derive newtype instance Show Identifier
derive newtype instance Eq Identifier

mkIdentifier :: String -> Maybe Identifier
mkIdentifier s = if not String.null s && allCharsLegal then Just $ Identifier s else Nothing
  where
  allCharsLegal = Array.all isLegalIdentifierChar $ String.CodeUnits.toCharArray s

isLegalIdentifierChar :: Char -> Boolean
isLegalIdentifierChar c = not $ Array.elem c [ '\\', 'λ', '\r', '\n', '\t', ' ', '(', ')', '.' ]

type Token = { pos :: Parsing.Position, element :: TokenElement }

data TokenElement
  = LeftParen
  | RightParen
  | Lambda
  | Dot
  | Name Identifier

derive instance Eq TokenElement
derive instance Generic TokenElement _

instance Show TokenElement where
  show = genericShow

isName :: Token -> Boolean
isName = case _ of
  { element: Name _ } -> true
  _ -> false

pureToken :: forall a. Parser String a -> TokenElement -> Parser String Token
pureToken parser element = do
  pos <- Parsing.position
  _ <- parser
  pure { pos, element }

tokens :: Parser String (List Token)
tokens = Parsing.many token

token :: Parser String Token
token = do
  Parsing.skipSpaces
  tok <- Parsing.choice [ leftParen, rightParen, backslashLambda, unicodeLambda, dot, name ]
  Parsing.skipSpaces
  pure tok

leftParen ∷ Parser String Token
leftParen = pureToken (Parsing.string "(") LeftParen

rightParen ∷ Parser String Token
rightParen = pureToken (Parsing.string ")") RightParen

backslashLambda ∷ Parser String Token
backslashLambda = pureToken (Parsing.string "\\") Lambda

unicodeLambda ∷ Parser String Token
unicodeLambda = pureToken (Parsing.string "λ") Lambda

dot ∷ Parser String Token
dot = pureToken (Parsing.string ".") Dot

name ∷ Parser String Token
name = do
  pos <- Parsing.position
  id <- identifier
  pure { pos, element: Name id }

identifier :: Parser String Identifier
identifier = do
  identChars <- Parsing.many1 (Parsing.satisfy isLegalIdentifierChar)
  let
    ident = identChars
      # NonEmptyList.toList
      # Array.fromFoldable
      # String.CodeUnits.fromCharArray
  ident
    # mkIdentifier
    # Parsing.liftMaybe (\_ -> "Illegal identifier: " <> ident)