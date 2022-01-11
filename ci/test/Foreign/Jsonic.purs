module Test.Foreign.Jsonic (jsonic) where

import Registry.Prelude

import Data.Argonaut.Core as Core
import Data.Either as Either
import Registry.Json (Json)
import Registry.Json as Json
import Test.Spec as Spec
import Test.Spec.Assertions as Assert

type Spec = Spec.SpecT Aff Unit Identity Unit

jsonic :: Spec
jsonic = do
  Spec.describe "Parses" do
    Spec.describe "Good json" goodJson
    Spec.describe "Bad json" badJson
  Spec.describe "Does not parse" do
    Spec.describe "Horrendous json" horrendousJson

parseTest :: String -> Json -> Spec
parseTest str json = Spec.it str do
  Json.parseJson str `Assert.shouldContain` Core.stringify json

goodJson :: Spec
goodJson = do
  let complexJson = Json.encode { complex: { nested: "json", bool: true } }
  parseTest "[1,2,3]" $ Json.encode [ 1, 2, 3 ]
  parseTest """{ "name": "test" }""" $ Json.encode { name: "test" }
  parseTest (Core.stringify complexJson) complexJson

badJson :: Spec
badJson = do
  parseTest "name: test" $ Json.encode { name: "test" }
  parseTest "{trailing: comma,}" $ Json.encode { trailing: "comma" }
  parseTest """[ "trailing comma", ]""" $ Json.encode [ "trailing comma" ]

horrendousJson :: Spec
horrendousJson = do
  let
    failParse str = Spec.it str do
      (Json.parseJson str :: Either String String) `Assert.shouldSatisfy` Either.isLeft

  failParse """{ "horrendously invalid json" }"""
