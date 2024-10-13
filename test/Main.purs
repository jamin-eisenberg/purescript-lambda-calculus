module Test.Main (main) where

import Prelude

import Ast (Expression(..), prettyPrint)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (fromJust)
import Effect (Effect)
import Effect.Exception (Error)
import Parse (parser)
import Parsing (ParseError(..), Position(..), runParser)
import Partial.Unsafe (unsafePartial)
import Test.Spec (describe, it, pending, pending')
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions as Spec
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Tokenize (Identifier, TokenElement(..), mkIdentifier, tokens)

ident ∷ String → Identifier
ident s = unsafePartial $ fromJust $ mkIdentifier s

var :: String -> Expression
var s = Variable $ ident s

shouldParseAs ∷ ∀ (m29 ∷ Type -> Type). MonadThrow Error m29 ⇒ String → Expression → m29 Unit
shouldParseAs expressionString expectedExpression =
  let
    parsed = do
      tokenized <- runParser expressionString tokens
      runParser tokenized parser
  in
    parsed `shouldEqual` (Right expectedExpression)

shouldNotParse ∷ ∀ (m47 ∷ Type -> Type). MonadThrow Error m47 ⇒ String → String → m47 Unit
shouldNotParse expressionString expectedErrorMessage =
  let
    parsed = do
      tokenized <- runParser expressionString tokens
      runParser tokenized parser
  in
    case parsed of
      Left (ParseError actualErrorMessage _) ->
        actualErrorMessage `shouldEqual` expectedErrorMessage
      Right expr -> Spec.fail $ "Parsed successfully, unexpectedly. Expr:\n\t" <> prettyPrint expr

shouldTokenizeAs ∷ ∀ (m3 ∷ Type -> Type). MonadThrow Error m3 ⇒ String → Array { element ∷ TokenElement, pos ∷ Position } → m3 Unit
shouldTokenizeAs expressionString expectedTokens =
  (Array.fromFoldable <$> runParser expressionString tokens) `shouldEqual` Right expectedTokens

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "purescript-lambda-calculus" do
    describe "tokenize" do
      it "gets both lambdas" do
        "\\ λ" `shouldTokenizeAs`
          [ { element: Lambda, pos: (Position { column: 1, index: 0, line: 1 }) }
          , { element: Lambda, pos: (Position { column: 3, index: 2, line: 1 }) }
          ]
      it "simple application" do
        "(x y)" `shouldTokenizeAs`
          [ { element: LeftParen, pos: (Position { column: 1, index: 0, line: 1 }) }
          , { element: (Name $ ident "x"), pos: (Position { column: 2, index: 1, line: 1 }) }
          , { element: (Name $ ident "y"), pos: (Position { column: 4, index: 3, line: 1 }) }
          , { element: RightParen, pos: (Position { column: 5, index: 4, line: 1 }) }
          ]
      it "complex" do
        "\\x y.λz x.(0 yz)" `shouldTokenizeAs`
          [ { element: Lambda, pos: (Position { column: 1, index: 0, line: 1 }) }
          , { element: (Name $ ident "x"), pos: (Position { column: 2, index: 1, line: 1 }) }
          , { element: (Name $ ident "y"), pos: (Position { column: 4, index: 3, line: 1 }) }
          , { element: Dot, pos: (Position { column: 5, index: 4, line: 1 }) }
          , { element: Lambda, pos: (Position { column: 6, index: 5, line: 1 }) }
          , { element: (Name $ ident "z"), pos: (Position { column: 7, index: 6, line: 1 }) }
          , { element: (Name $ ident "x"), pos: (Position { column: 9, index: 8, line: 1 }) }
          , { element: Dot, pos: (Position { column: 10, index: 9, line: 1 }) }
          , { element: LeftParen, pos: (Position { column: 11, index: 10, line: 1 }) }
          , { element: (Name $ ident "0"), pos: (Position { column: 12, index: 11, line: 1 }) }
          , { element: (Name $ ident "yz"), pos: (Position { column: 14, index: 13, line: 1 }) }
          , { element: RightParen, pos: (Position { column: 16, index: 15, line: 1 }) }
          ]

    describe "parse" do
      it "plain variable" do
        "x" `shouldParseAs` var "x"
      it "identity" do
        "\\x.x" `shouldParseAs` Abstraction (ident "x") (var "x")
      it "const apart" do
        "\\x.\\y.x" `shouldParseAs` Abstraction (ident "x") (Abstraction (ident "y") (var "x"))
      it "const together" do
        "\\x y.x" `shouldParseAs` Abstraction (ident "x") (Abstraction (ident "y") (var "x"))
    it "multiple parameters in function body" do
      "\\x y.\\z x.x" `shouldParseAs`
        Abstraction (ident "x")
          ( Abstraction (ident "y")
              ( Abstraction (ident "z")
                  ( Abstraction (ident "x")
                      (var "x")
                  )
              )
          )

    it "basic application" do
      "(x y)" `shouldParseAs` Application (var "x") (var "y")
    it "nested operator application" do
      "((x z) y)" `shouldParseAs` Application (Application (var "x") (var "z")) (var "y")
    it "nested operand application" do
      "(x (y z))" `shouldParseAs` Application (var "x") (Application (var "y") (var "z"))
    it "both-nested application" do
      "((x z) (y z))" `shouldParseAs`
        Application
          (Application (var "x") (var "z"))
          (Application (var "y") (var "z"))

    pending' "no parens basic application" do
      "x y" `shouldParseAs` Application (var "x") (var "y")
    pending' "no parens left-associative application" do
      "x y z" `shouldParseAs` Application (Application (var "x") (var "y")) (var "z")
    it "λ symbol allowed as abstraction start" do
      "λx.x" `shouldParseAs` Abstraction (ident "x") (var "x")
    it "λ disallowed as variable" do
      "λ" `shouldNotParse` "Unexpected EOF"

    describe "interpret" do
      pending "todo"