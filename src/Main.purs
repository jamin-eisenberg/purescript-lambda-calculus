module Main (main) where

import Prelude

import Ast (canonicalize)
import Data.Either as Either
import Data.String as String
import Effect (Effect)
import Effect.Class.Console as Console
import Parse as Parse
import Parsing as Parsing
import Parsing.String as Parsing.String
import Tokenize as Tokenize

main :: Effect Unit
main = do
  let input = "λz y x. ((x z) (y z))"
  -- let input = "λz. λy. ((y λx. x) λx. (z x))"
  let
    parseResult = do
      tokens <- Parsing.runParser input Tokenize.tokens
      Parsing.runParser tokens Parse.parser
  let
    output =
      Either.either
        (Parsing.String.parseErrorHuman input 100 >>> String.joinWith "\n")
        (canonicalize >>> show)
        parseResult
  Console.log output

