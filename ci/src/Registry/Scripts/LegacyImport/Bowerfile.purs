module Registry.Scripts.LegacyImport.Bowerfile
  ( Bowerfile(..)
  , toManifestFields
  ) where

import Registry.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Map as Map
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Registry.Json (class RegistryJson, Json, (.:?))
import Registry.Json as Json
import Registry.Scripts.LegacyImport.ManifestFields (ManifestFields)

toManifestFields :: Bowerfile -> ManifestFields
toManifestFields (Bowerfile fields) = fields

newtype Bowerfile = Bowerfile ManifestFields

derive newtype instance Eq Bowerfile
derive newtype instance Show Bowerfile

instance RegistryJson Bowerfile where
  encode (Bowerfile fields) = Json.encode fields

  decode json = do
    obj <- Json.decode json
    description <- obj .:? "description"
    license <- decodeStringOrStringArray obj "license"
    dependencies <- fromMaybe Map.empty <$> obj .:? "dependencies"
    devDependencies <- fromMaybe Map.empty <$> obj .:? "devDependencies"
    pure $ Bowerfile { description, license, dependencies, devDependencies }

decodeStringOrStringArray
  :: Object Json
  -> String
  -> Either String (Maybe (NonEmptyArray NonEmptyString))
decodeStringOrStringArray obj fieldName = do
  value <- obj .:? fieldName
  case value of
    Nothing -> pure Nothing
    Just v -> do
      decoded <- (Json.decode v <#> Array.singleton) <|> Json.decode v
      pure $ NEA.fromArray $ Array.catMaybes $ map NES.fromString decoded
