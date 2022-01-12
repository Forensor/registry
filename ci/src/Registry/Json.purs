-- | This module exports a class for encoding and decoding JSON according to
-- | Registry preferences.
-- |
-- | We don't use Argonaut type classes because we differ from its choice of
-- | encoding and decoding for common types like `Maybe`, and `Map`, aiming for
-- | idiomatic JSON as output rather than preserving PureScript constructors.
-- |
-- | The choice also allows us to define instances for types we don't define,
-- | like `RFC3339String` and `BigInt`, without newtypes.
module Registry.Json
  ( module Exports
  , class RegistryJson
  , printJson
  , parseJson
  , writeJsonFile
  , readJsonFile
  , encodeObject
  , putField
  , (:=)
  , decodeObject
  , readField
  , getField
  , (.:)
  , getField'
  , (.:?)
  , getFieldDefault
  , (.?=)
  , encode
  , decode
  , decodeWithParser
  -- Required for instances, but not intended for user code
  , class EncodeRecord
  , encodeRecord
  , class DecodeRecord
  , decodeRecord
  , class EncodeRecordField
  , encodeRecordField
  , class DecodeRecordField
  , decodeRecordField
  ) where

import Registry.Prelude

import Control.Monad.Reader (Reader, ask, runReader)
import Control.Monad.State (State, runState)
import Control.Monad.State as State
import Data.Argonaut.Core (Json) as Exports
import Data.Argonaut.Core as Core
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Int as Int
import Data.Map as Map
import Data.Newtype (unwrap, wrap)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Data.Symbol (class IsSymbol)
import Data.Symbol as Symbol
import Foreign.Jsonic as Jsonic
import Foreign.Object as Object
import Node.FS.Aff as FS
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser as SP
import Type.Proxy (Proxy(..))

-- | Print a type as a formatted JSON string
printJson :: forall a. RegistryJson a => a -> String
printJson = Core.stringifyWithIndent 2 <<< encode

-- | Parse a type from a string of JSON data
parseJson :: forall a. RegistryJson a => String -> Either String a
parseJson = decode <=< Jsonic.parseJsonic

-- | Encode data as JSON and write it to the provided filepath
writeJsonFile :: forall a. RegistryJson a => FilePath -> a -> Aff Unit
writeJsonFile path = FS.writeTextFile UTF8 path <<< printJson

-- | Decode data from a JSON file at the provided filepath
readJsonFile :: forall a. RegistryJson a => FilePath -> Aff (Either String a)
readJsonFile path = map parseJson $ FS.readTextFile UTF8 path

-- | Decode JSON using a string parser
decodeWithParser :: forall a. Parser a -> Core.Json -> Either String a
decodeWithParser parser json = do
  string <- decode json
  parsed <- lmap SP.printParserError $ SP.runParser parser string
  pure parsed

encodeObject :: State (Object Core.Json) Unit -> Core.Json
encodeObject encoder = encode $ snd $ runState encoder Object.empty

putField :: forall a. RegistryJson a => String -> a -> State (Object Core.Json) Unit
putField key value = do
  let encoded = encode value
  unless (Core.isNull encoded) do
    State.modify_ (Object.insert key (encode value))

infix 7 putField as :=

decodeObject :: forall a. Object Core.Json -> Reader (Object Core.Json) (Either String a) -> Either String a
decodeObject = flip runReader

readField :: forall a. RegistryJson a => String -> Reader (Object Core.Json) (Either String a)
readField key = ask >>= \obj -> pure (obj .: key)

-- | Look up and decode a field in an object, failing if it is not there.
getField :: forall a. RegistryJson a => Object Core.Json -> String -> Either String a
getField object key = maybe (Left $ "Expected value at key: '" <> key <> "'") decode (Object.lookup key object)

infix 7 getField as .:

-- | Look up and decode a field in an object, returning `Maybe` if it is not there.
getField' :: forall a. RegistryJson a => Object Core.Json -> String -> Either String (Maybe a)
getField' object key = maybe (pure Nothing) decode' (Object.lookup key object)
  where
  decode' json = if Core.isNull json then pure Nothing else map Just (decode json)

infix 7 getField' as .:?

-- | Look up and decode a field in an object, defaulting if it isn't found
getFieldDefault :: forall a. Monoid a => RegistryJson a => Object Core.Json -> String -> Either String a
getFieldDefault object = map (fromMaybe mempty) <<< getField' object

infix 7 getFieldDefault as .?=

class RegistryJson a where
  encode :: a -> Core.Json
  decode :: Core.Json -> Either String a

instance decodeJsonJson :: RegistryJson Core.Json where
  encode = identity
  decode = Right

instance RegistryJson Boolean where
  encode = Core.fromBoolean
  decode = Core.caseJsonBoolean (Left "Expected Boolean") Right

instance RegistryJson String where
  encode = Core.fromString
  decode = Core.caseJsonString (Left "Expected String") Right

instance RegistryJson Number where
  encode = Core.fromNumber
  decode = Core.caseJsonNumber (Left "Expected Number") Right

instance RegistryJson Int where
  encode = Core.fromNumber <<< Int.toNumber
  decode = note "Expected Int" <<< Int.fromNumber <=< decode

instance RegistryJson a => RegistryJson (Array a) where
  encode = Core.fromArray <<< map encode
  decode = Core.caseJsonArray (Left "Expected Array") (traverse decode)

instance RegistryJson a => RegistryJson (Object a) where
  encode = Core.fromObject <<< map encode
  decode = Core.caseJsonObject (Left "Expected Object") (traverse decode)

instance RegistryJson a => RegistryJson (Maybe a) where
  encode = case _ of
    Nothing -> Core.jsonNull
    Just value -> encode value
  decode json
    | Core.isNull json = Right Nothing
    | otherwise = map Just $ decode json

instance (RegistryJson e, RegistryJson a) => RegistryJson (Either e a) where
  encode = either encode encode
  decode json =
    case decode json of
      Right right -> Right (Right right)
      Left rightError -> case decode json of
        Right left -> Right (Left left)
        Left leftError -> Left $ Array.fold
          [ "Expected Either: failed Left ("
          , leftError
          , ") failed Right ("
          , rightError
          , ")"
          ]

instance RegistryJson NonEmptyString where
  encode = encode <<< NES.toString
  decode = decode >=> NES.fromString >>> note "Expected NonEmptyString"

instance RegistryJson a => RegistryJson (NonEmptyArray a) where
  encode = encode <<< NEA.toArray
  decode = decode >=> NEA.fromArray >>> note "Expected NonEmptyArray"

instance RegistryJson a => RegistryJson (Map String a) where
  encode = encode <<< Object.fromFoldable <<< (Map.toUnfoldableUnordered :: _ -> Array _)
  decode = map (Map.fromFoldable <<< (Object.toUnfoldable :: _ -> Array _)) <=< decode

else instance (Ord k, Newtype k String, RegistryJson v) => RegistryJson (Map k v) where
  encode = encode <<< Object.fromFoldable <<< map (lmap unwrap) <<< (Map.toUnfoldableUnordered :: _ -> Array _)
  decode = map (Map.fromFoldable <<< map (lmap wrap) <<< (Object.toUnfoldable :: _ -> Array _)) <=< decode

instance (EncodeRecord row list, DecodeRecord row list, RL.RowToList row list) => RegistryJson (Record row) where
  encode record = Core.fromObject $ encodeRecord record (Proxy :: Proxy list)
  decode json = case Core.toObject json of
    Nothing -> Left "Expected Object"
    Just object -> decodeRecord object (Proxy :: Proxy list)

---------

class EncodeRecord (row :: Row Type) (list :: RL.RowList Type) where
  encodeRecord :: Record row -> Proxy list -> Object Core.Json

instance EncodeRecord row RL.Nil where
  encodeRecord _ _ = Object.empty

instance (EncodeRecordField value, RegistryJson value, EncodeRecord row tail, IsSymbol field, Row.Cons field value tail' row) => EncodeRecord row (RL.Cons field value tail) where
  encodeRecord row _ = do
    let
      _field = Proxy :: Proxy field
      fieldName = Symbol.reflectSymbol _field
      fieldValue = Record.get _field row
      object = encodeRecord row (Proxy :: Proxy tail)

    encodeRecordField fieldName fieldValue object

class DecodeRecord (row :: Row Type) (list :: RL.RowList Type) | list -> row where
  decodeRecord :: Object Core.Json -> Proxy list -> Either String (Record row)

instance DecodeRecord () RL.Nil where
  decodeRecord _ _ = Right {}

instance (DecodeRecordField value, DecodeRecord rowTail tail, IsSymbol field, Row.Cons field value rowTail row, Row.Lacks field rowTail) => DecodeRecord row (RL.Cons field value tail) where
  decodeRecord object _ = do
    let
      _field = Proxy :: Proxy field
      fieldName = Symbol.reflectSymbol _field

    case decodeRecordField (Object.lookup fieldName object) of
      Nothing ->
        Left $ "Expected field: '" <> fieldName <> "'"
      Just fieldValue -> do
        val <- fieldValue
        rest <- decodeRecord object (Proxy :: Proxy tail)
        Right $ Record.insert _field val rest

-- This class ensures that `Maybe` values are not included when JSON-encoding
-- records.
class EncodeRecordField a where
  encodeRecordField :: String -> a -> Object Core.Json -> Object Core.Json

instance RegistryJson a => EncodeRecordField (Maybe a) where
  encodeRecordField key = case _ of
    Nothing -> identity
    Just value -> Object.insert key (encode value)

else instance RegistryJson a => EncodeRecordField a where
  encodeRecordField key value = Object.insert key (encode value)

class DecodeRecordField a where
  decodeRecordField :: Maybe Core.Json -> Maybe (Either String a)

instance RegistryJson a => DecodeRecordField (Maybe a) where
  decodeRecordField = Just <<< case _ of
    Nothing -> Right Nothing
    Just json | Core.isNull json -> Right Nothing
    Just json -> decode json

else instance RegistryJson a => DecodeRecordField a where
  decodeRecordField = map decode
