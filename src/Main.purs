module Main (main) where

import Prelude

import Data.Either as Either
import Data.String as String
import Effect (Effect)
import Effect.Class.Console as Console
import Parser (parser)
import Parsing as Parsing
import Parsing.String as Parsing.String

main :: Effect Unit
main = do
  let input = "z x"
  let parseResult = Parsing.runParser input parser
  let
    output = Either.either
      (Parsing.String.parseErrorHuman input 20 >>> String.joinWith "\n")
      show
      parseResult
  Console.log output

