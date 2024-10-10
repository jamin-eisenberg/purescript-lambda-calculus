module Test.Main where

import Prelude

import Ast (Expression(..), Identifier, mkIdentifier)
import Data.Either (Either(..))
import Data.Maybe (fromJust)
import Effect (Effect)
import Parser (parser)
import Parsing (runParser)
import Partial.Unsafe (unsafePartial)
import Test.Spec (describe, it, pending)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

ident ∷ String → Identifier
ident s = unsafePartial $ fromJust $ mkIdentifier s

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "purescript-lambda-calculus" do
    describe "parse" do
      it "plain variable" do
        runParser "x" parser `shouldEqual` Right (Variable $ ident "x")

    describe "interpret" do
      pending "todo"