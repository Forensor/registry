module Registry.Scripts.LegacyImport.SpagoJson
  ( SpagoJson
  , toManifestFields
  ) where

import Registry.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Map as Map
import Data.String.NonEmpty (NonEmptyString)
import Registry.Json (class RegistryJson, (.:?), (.?=))
import Registry.Json as Json
import Registry.Scripts.LegacyImport.Error (RawPackageName, RawVersion)
import Registry.Scripts.LegacyImport.ManifestFields (ManifestFields)

toManifestFields :: SpagoJson -> ManifestFields
toManifestFields (SpagoJson { license, dependencies, packages }) =
  { license: map NEA.singleton license
  , dependencies: packageDependencies
  , devDependencies: Map.empty
  , description: Nothing
  }
  where
  packageDependencies :: Map RawPackageName RawVersion
  packageDependencies = do
    let
      foldFn m name = fromMaybe m do
        { version } <- Map.lookup name packages
        pure $ Map.insert name version m

    Array.foldl foldFn Map.empty dependencies

-- | The output of calling `dhall-to-json` on a `spago.dhall` file
newtype SpagoJson = SpagoJson
  { license :: Maybe NonEmptyString
  , dependencies :: Array RawPackageName
  -- The packages available in the package set
  , packages :: Map RawPackageName SpagoPackage
  }

derive newtype instance Eq SpagoJson

type SpagoPackage =
  { version :: RawVersion
  }

instance RegistryJson SpagoJson where
  encode (SpagoJson spago) = Json.encode spago

  decode json = do
    object <- Json.decode json
    license <- object .:? "license"
    dependencies <- object .?= "dependencies"
    packages <- object .:? "packages"
    pure $ SpagoJson { license, dependencies, packages: fromMaybe Map.empty packages }
