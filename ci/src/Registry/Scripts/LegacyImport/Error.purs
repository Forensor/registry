module Registry.Scripts.LegacyImport.Error where

import Registry.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Int as Int
import Data.Interpolate (i)
import Data.List.NonEmpty as NEL
import Data.String as String
import Data.String.CodeUnits as CU
import Data.String.CodeUnits as SCU
import Registry.Json (class RegistryJson)
import Registry.Json as Json
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Utils.Parser as RP
import Text.Parsing.StringParser as SP
import Text.Parsing.StringParser.CodePoints as SPCP
import Text.Parsing.StringParser.Combinators as SPC

-- | A map of error types to package names to package versions, where failed
-- | versions contain rich information about why they failed.
newtype PackageFailures = PackageFailures (Map ImportErrorKey (Map RawPackageName (Either ImportError (Map RawVersion ImportError))))

derive instance Newtype PackageFailures _

derive newtype instance RegistryJson PackageFailures

-- | An import error printed as a key usable in a map
newtype ImportErrorKey = ImportErrorKey String

derive instance Newtype ImportErrorKey _
derive newtype instance Eq ImportErrorKey
derive newtype instance Ord ImportErrorKey
instance Show ImportErrorKey where
  show (ImportErrorKey key) = i "(ImportErrorKey " key ")"

-- | An unprocessed package name, which may possibly be malformed.
newtype RawPackageName = RawPackageName String

derive instance Newtype RawPackageName _
derive newtype instance Eq RawPackageName
derive newtype instance Ord RawPackageName
derive newtype instance RegistryJson RawPackageName
derive newtype instance Show RawPackageName

-- | An unprocessed version, taken from a GitHub tag
newtype RawVersion = RawVersion String

derive instance Newtype RawVersion _
derive newtype instance Eq RawVersion
derive newtype instance Ord RawVersion
derive newtype instance RegistryJson RawVersion
derive newtype instance Show RawVersion

-- | An error representing why a package version cannot be imported from the
-- | Bower registry.
data ImportError
  = InvalidGitHubRepo String
  | ResourceError ResourceError
  | MalformedPackageName String
  | NoDependencyFiles
  | NonRegistryDependencies (NonEmptyArray RawPackageName)
  | NoManifests
  | ManifestImportError (NonEmptyArray ManifestError)

derive instance Eq ImportError

instance RegistryJson ImportError where
  encode = unsafeCrashWith "blah"
  decode = unsafeCrashWith "blah"

manifestErrorKey :: ImportErrorKey
manifestErrorKey = ImportErrorKey "manifestError"

printImportErrorKey :: ImportError -> ImportErrorKey
printImportErrorKey = case _ of
  InvalidGitHubRepo _ -> ImportErrorKey "invalidGitHubRepo"
  ResourceError _ -> ImportErrorKey "resourceError"
  MalformedPackageName _ -> ImportErrorKey "malformedPackageName"
  NoDependencyFiles -> ImportErrorKey "noDependencyFiles"
  NonRegistryDependencies _ -> ImportErrorKey "nonRegistryDependencies"
  NoManifests -> ImportErrorKey "noManifests"
  ManifestImportError _ -> manifestErrorKey

-- | An error fetching a resource necessary to produce a Manifest for a
-- | given package.
type ResourceError = { resource :: Either APIResource FileResource, error :: RequestError }

data RequestError = BadRequest | BadStatus Int | DecodeError String

derive instance Eq RequestError

instance RegistryJson RequestError where
  encode = Json.encode <<< case _ of
    BadRequest -> "BadRequest"
    BadStatus status -> "BadStatus (" <> Int.toStringAs Int.decimal status <> ")"
    DecodeError error -> "DecodeError (" <> error <> ")"
  decode = Json.decodeWithParser do
    let
      badRequest = RP.nullaryCtor "BadRequest" BadRequest
      badStatus = RP.singleCtor "BadStatus" BadStatus do
        statusString <- RP.parens RP.anyString
        case Int.fromStringAs Int.decimal statusString of
          Nothing -> SP.fail $ "Expected Int status code, received: " <> statusString
          Just value -> pure value
      decodeError = RP.singleCtor "DecodeError" DecodeError (RP.parens RP.anyString)

    RP.tryChoice [ badRequest, badStatus, decodeError ]

-- | An error representing why a manifest could not be produced for this package
data ManifestError
  = MissingName
  | MissingLicense
  | BadLicenses (Array String)
  | BadVersion String
  | InvalidDependencyNames (NonEmptyArray String)
  | BadDependencyVersions (NonEmptyArray { dependency :: PackageName, failedBounds :: String })

derive instance Eq ManifestError

instance RegistryJson ManifestError where
  encode = Json.encode <<< case _ of
    MissingName -> "MissingName"
    MissingLicense -> "MissingLicense"
    BadLicenses arr ->
      "BadLicenses [ " <> String.joinWith ", " arr <> " ]"
    BadVersion str ->
      "BadVersion (" <> str <> ")"
    InvalidDependencyNames arr ->
      "InvalidDependencyNames [ " <> String.joinWith ", " (NEA.toArray arr) <> " ]"
    BadDependencyVersions arr -> do
      let print { dependency, failedBounds } = PackageName.print dependency <> "@" <> failedBounds
      "BadDependencyVersions [ " <> String.joinWith ", " (NEA.toArray (map print arr)) <> " ]"

  decode = Json.decodeWithParser do
    let
      missingName = RP.nullaryCtor "MissingName" MissingName
      missingLicense = RP.nullaryCtor "MissingLicense" MissingLicense
      badLicenses = RP.singleCtor "BadLicenses" BadLicenses (RP.array RP.anyString)
      badVersion = RP.singleCtor "BadVersion" BadVersion (RP.parens RP.anyString)

      invalidDependencyNames =
        RP.singleCtor "InvalidDependencyNames" InvalidDependencyNames (RP.array1 RP.anyString)

      badDependencyVersions = RP.singleCtor "BadDependencyVersions" BadDependencyVersions do
        let
          parseDependency = do
            dependency <- do
              rawPackage <- SPC.many1Till SPCP.anyChar (SPCP.char '@')
              let packageStr = SCU.fromCharArray $ NEL.toUnfoldable rawPackage
              either (SP.fail <<< _.error) pure (PackageName.parse packageStr)

            _ <- SPCP.char '@'

            failedBounds <- do
              bounds <- SPC.manyTill SPCP.anyChar (SPCP.char ',')
              pure $ CU.fromCharArray $ Array.fromFoldable bounds

            pure { dependency, failedBounds }

        RP.array1 parseDependency

    RP.tryChoice
      [ missingName
      , missingLicense
      , badLicenses
      , badVersion
      , invalidDependencyNames
      , badDependencyVersions
      ]

newtype ManifestErrorKey = ManifestErrorKey String

derive instance Newtype ManifestErrorKey _
derive newtype instance Eq ManifestErrorKey
derive newtype instance Ord ManifestErrorKey

instance Show ManifestErrorKey where
  show (ManifestErrorKey key) = i "(ManifestErrorKey " key ")"

printManifestErrorKey :: ManifestError -> ManifestErrorKey
printManifestErrorKey = ManifestErrorKey <<< case _ of
  MissingName -> "missingName"
  MissingLicense -> "missingLicense"
  BadLicenses _ -> "badLicenses"
  BadVersion _ -> "badVersion"
  InvalidDependencyNames _ -> "invalidDependencyNames"
  BadDependencyVersions _ -> "badDependencyVersions"

-- | A resource that has to be fetched via an API
data APIResource = GitHubReleases

derive instance Eq APIResource

instance RegistryJson APIResource where
  encode = Json.encode <<< case _ of GitHubReleases -> "GitHubReleases"
  decode = Json.decode >=> case _ of
    "GitHubReleases" -> Right GitHubReleases
    str -> Left $ "Invalid APIResource: (" <> str <> ")"

-- | A resource that has to be fetched via donwloading the relevant file
data FileResource = BowerJson | SpagoDhall | PackagesDhall | PackageJson | LicenseFile

derive instance Eq FileResource

instance RegistryJson FileResource where
  encode = Json.encode <<< fileResourcePath
  decode = Json.decode >=> case _ of
    "bower.json" -> Right BowerJson
    "spago.dhall" -> Right SpagoDhall
    "packages.dhall" -> Right PackagesDhall
    "package.json" -> Right PackageJson
    "LICENSE" -> Right LicenseFile
    other -> Left $ "Invalid FileResource: (" <> other <> ")"

fileResourcePath :: FileResource -> FilePath
fileResourcePath = case _ of
  BowerJson -> "bower.json"
  SpagoDhall -> "spago.dhall"
  PackagesDhall -> "packages.dhall"
  PackageJson -> "package.json"
  LicenseFile -> "LICENSE"
